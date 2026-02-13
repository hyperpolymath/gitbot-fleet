"""Materials information commands cog."""

import discord
from discord.ext import commands

from models import get_db
from services.database import MaterialService, UserService
from services.sustainability import SustainabilityAnalyzer
from utils.logger import get_logger

logger = get_logger(__name__)


class MaterialsCommands(commands.Cog):
    """Commands for material information."""

    def __init__(self, bot: commands.Bot):
        self.bot = bot
        self.analyzer = SustainabilityAnalyzer()

    @commands.command(name="impact", aliases=["material"])
    async def material_impact(self, ctx: commands.Context, *, material_name: str):
        """
        Get environmental impact of a material.

        Usage: !impact <material_name>
        Example: !impact cotton
        """
        async with ctx.typing():
            db = next(get_db())
            try:
                # Track user
                user = UserService.get_or_create(db, ctx.author.id, ctx.author.name)
                user.add_points(5)
                UserService.update(db, user)

                # Find material
                material = MaterialService.get_by_name(db, material_name)

                if not material:
                    await ctx.send(f"❌ Material '{material_name}' not found.")
                    return

                # Calculate scores
                overall_score = material.calculate_overall_score()
                grade = material.get_grade()
                category = self.analyzer.get_impact_category(overall_score)
                recommendation = self.analyzer.get_material_recommendation(material)

                # Create embed
                embed = discord.Embed(
                    title=f"🧵 {material.name}",
                    description=material.description or "No description available",
                    color=discord.Color.green() if overall_score >= 60 else discord.Color.orange()
                )

                embed.add_field(
                    name="Overall Score",
                    value=f"**{overall_score:.1f}/100** (Grade: {grade})",
                    inline=False
                )

                embed.add_field(
                    name="Type",
                    value=material.material_type.value.replace("_", " ").title(),
                    inline=True
                )

                # Environmental scores
                embed.add_field(
                    name="💧 Water Usage",
                    value=f"{material.water_usage_score:.1f}/100",
                    inline=True
                )
                embed.add_field(
                    name="🌍 Carbon Footprint",
                    value=f"{material.carbon_footprint_score:.1f}/100",
                    inline=True
                )
                embed.add_field(
                    name="♻️ Biodegradability",
                    value=f"{material.biodegradability_score:.1f}/100",
                    inline=True
                )
                embed.add_field(
                    name="🧪 Chemical Usage",
                    value=f"{material.chemical_usage_score:.1f}/100",
                    inline=True
                )
                embed.add_field(
                    name="⚡ Energy Consumption",
                    value=f"{material.energy_consumption_score:.1f}/100",
                    inline=True
                )

                # Production metrics
                if material.water_liters_per_kg:
                    embed.add_field(
                        name="Water per kg",
                        value=f"{material.water_liters_per_kg:.0f} L",
                        inline=True
                    )
                if material.co2_kg_per_kg:
                    embed.add_field(
                        name="CO2 per kg",
                        value=f"{material.co2_kg_per_kg:.2f} kg",
                        inline=True
                    )

                # Additional properties
                if material.is_biodegradable:
                    embed.add_field(
                        name="Biodegradable",
                        value=material.is_biodegradable,
                        inline=True
                    )
                if material.recycling_potential:
                    embed.add_field(
                        name="Recycling Potential",
                        value=material.recycling_potential,
                        inline=True
                    )

                # Recommendation
                embed.add_field(
                    name="Recommendation",
                    value=recommendation,
                    inline=False
                )

                embed.set_footer(text=f"Requested by {ctx.author.name} | +5 points")

                await ctx.send(embed=embed)

            except Exception as e:
                logger.error(f"Error in impact command: {e}", exc_info=True)
                await ctx.send("❌ An error occurred while fetching material data.")
            finally:
                db.close()

    @commands.command(name="compare")
    async def compare_materials(
        self, ctx: commands.Context, material1: str, material2: str
    ):
        """
        Compare two materials.

        Usage: !compare <material1> <material2>
        Example: !compare cotton polyester
        """
        async with ctx.typing():
            db = next(get_db())
            try:
                # Track user
                user = UserService.get_or_create(db, ctx.author.id, ctx.author.name)
                user.add_points(7)
                UserService.update(db, user)

                # Find materials
                mat1 = MaterialService.get_by_name(db, material1)
                mat2 = MaterialService.get_by_name(db, material2)

                if not mat1:
                    await ctx.send(f"❌ Material '{material1}' not found.")
                    return
                if not mat2:
                    await ctx.send(f"❌ Material '{material2}' not found.")
                    return

                # Compare
                comparison = self.analyzer.compare_materials(mat1, mat2)

                # Create embed
                embed = discord.Embed(
                    title=f"⚖️ {mat1.name} vs {mat2.name}",
                    description="Material Comparison",
                    color=discord.Color.blue()
                )

                # Overall scores
                score1 = comparison["overall_scores"][mat1.name]
                score2 = comparison["overall_scores"][mat2.name]
                winner = mat1.name if score1 > score2 else mat2.name

                embed.add_field(
                    name="Overall Scores",
                    value=f"**{mat1.name}**: {score1:.1f}/100\n**{mat2.name}**: {score2:.1f}/100\n\n🏆 **Winner**: {winner}",
                    inline=False
                )

                # Category comparisons
                categories = [
                    ("💧 Water Usage", "water_usage"),
                    ("🌍 Carbon Footprint", "carbon_footprint"),
                    ("♻️ Biodegradability", "biodegradability")
                ]

                for emoji_name, key in categories:
                    data = comparison[key]
                    embed.add_field(
                        name=emoji_name,
                        value=f"{mat1.name}: {data[mat1.name]:.1f}\n{mat2.name}: {data[mat2.name]:.1f}\n✓ {data['better']}",
                        inline=True
                    )

                embed.set_footer(text=f"Requested by {ctx.author.name} | +7 points")

                await ctx.send(embed=embed)

            except Exception as e:
                logger.error(f"Error in compare command: {e}", exc_info=True)
                await ctx.send("❌ An error occurred while comparing materials.")
            finally:
                db.close()

    @commands.command(name="search")
    async def search(self, ctx: commands.Context, *, query: str):
        """
        Search for garments or materials.

        Usage: !search <query>
        Example: !search organic cotton
        """
        async with ctx.typing():
            db = next(get_db())
            try:
                from services.database import GarmentService

                # Search both garments and materials
                materials = MaterialService.search(db, query)
                garments = GarmentService.search(db, query)

                if not materials and not garments:
                    await ctx.send(f"❌ No results found for '{query}'.")
                    return

                embed = discord.Embed(
                    title=f"🔍 Search Results for '{query}'",
                    color=discord.Color.blue()
                )

                if materials:
                    materials_text = "\n".join(
                        f"• {m.name} ({m.material_type.value})"
                        for m in materials[:5]
                    )
                    embed.add_field(
                        name=f"Materials ({len(materials)})",
                        value=materials_text,
                        inline=False
                    )

                if garments:
                    garments_text = "\n".join(
                        f"• {g.name} ({g.category})"
                        for g in garments[:5]
                    )
                    embed.add_field(
                        name=f"Garments ({len(garments)})",
                        value=garments_text,
                        inline=False
                    )

                embed.set_footer(text=f"Requested by {ctx.author.name}")

                await ctx.send(embed=embed)

            except Exception as e:
                logger.error(f"Error in search command: {e}", exc_info=True)
                await ctx.send("❌ An error occurred during search.")
            finally:
                db.close()


async def setup(bot: commands.Bot):
    """Set up the cog."""
    await bot.add_cog(MaterialsCommands(bot))
