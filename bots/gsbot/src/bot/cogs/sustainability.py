"""Sustainability commands cog."""

import discord
from discord.ext import commands
from typing import Optional

from models import get_db, Garment
from services.database import GarmentService, MaterialService, UserService
from services.sustainability import SustainabilityAnalyzer
from utils.logger import get_logger

logger = get_logger(__name__)


class SustainabilityCommands(commands.Cog):
    """Commands for sustainability information."""

    def __init__(self, bot: commands.Bot):
        self.bot = bot
        self.analyzer = SustainabilityAnalyzer()

    @commands.command(name="sustainability", aliases=["sus", "score"])
    async def sustainability_score(self, ctx: commands.Context, *, garment_name: str):
        """
        Get sustainability score for a garment.

        Usage: !sustainability <garment_name>
        Example: !sustainability cotton t-shirt
        """
        async with ctx.typing():
            db = next(get_db())
            try:
                # Track user
                user = UserService.get_or_create(
                    db, ctx.author.id, ctx.author.name
                )
                user.add_points(5)
                UserService.update(db, user)

                # Find garment
                garment = GarmentService.get_by_name(db, garment_name)

                if not garment:
                    await ctx.send(
                        f"❌ Garment '{garment_name}' not found. "
                        f"Try `!search {garment_name}` to find similar items."
                    )
                    return

                # Calculate score
                score = garment.calculate_sustainability_score()
                impact = garment.get_environmental_impact()
                category = self.analyzer.get_impact_category(score)

                # Create embed
                embed = discord.Embed(
                    title=f"🌱 {garment.name}",
                    description=garment.description or "No description available",
                    color=discord.Color.green() if score >= 60 else discord.Color.orange()
                )

                embed.add_field(
                    name="Sustainability Score",
                    value=f"**{score:.1f}/100** - {category}",
                    inline=False
                )

                # Add environmental impact
                embed.add_field(
                    name="💧 Water Usage",
                    value=impact.get("water_usage", "Unknown"),
                    inline=True
                )
                embed.add_field(
                    name="🌍 Carbon Footprint",
                    value=impact.get("carbon_footprint", "Unknown"),
                    inline=True
                )
                embed.add_field(
                    name="⚡ Energy",
                    value=impact.get("energy_consumption", "Unknown"),
                    inline=True
                )

                # Add materials
                if garment.materials:
                    materials_text = "\n".join(
                        f"• {m.name} ({m.material_type.value})"
                        for m in garment.materials
                    )
                    embed.add_field(
                        name="Materials",
                        value=materials_text,
                        inline=False
                    )

                # Add lifespan info
                if garment.expected_lifespan_years:
                    embed.add_field(
                        name="Expected Lifespan",
                        value=f"{garment.expected_lifespan_years} years",
                        inline=True
                    )

                embed.set_footer(text=f"Requested by {ctx.author.name} | +5 points")

                await ctx.send(embed=embed)

            except Exception as e:
                logger.error(f"Error in sustainability command: {e}", exc_info=True)
                await ctx.send("❌ An error occurred while fetching sustainability data.")
            finally:
                db.close()

    @commands.command(name="alternatives", aliases=["alt"])
    async def alternatives(self, ctx: commands.Context, *, garment_name: str):
        """
        Find sustainable alternatives for a garment.

        Usage: !alternatives <garment_name>
        Example: !alternatives polyester jacket
        """
        async with ctx.typing():
            db = next(get_db())
            try:
                # Track user
                user = UserService.get_or_create(db, ctx.author.id, ctx.author.name)
                user.add_points(5)
                UserService.update(db, user)

                # Find garment
                garment = GarmentService.get_by_name(db, garment_name)

                if not garment:
                    await ctx.send(f"❌ Garment '{garment_name}' not found.")
                    return

                # Get alternatives
                alternatives = GarmentService.get_alternatives(db, garment)

                if not alternatives:
                    await ctx.send(
                        f"✅ Great news! '{garment.name}' is already one of the most "
                        "sustainable options in its category."
                    )
                    return

                # Create embed
                embed = discord.Embed(
                    title=f"♻️ Sustainable Alternatives to {garment.name}",
                    description=f"Here are more sustainable options in the {garment.category} category:",
                    color=discord.Color.green()
                )

                for alt in alternatives[:5]:
                    alt_score = alt.calculate_sustainability_score()
                    embed.add_field(
                        name=f"{alt.name} - Score: {alt_score:.1f}/100",
                        value=alt.description or "No description",
                        inline=False
                    )

                embed.set_footer(text=f"Requested by {ctx.author.name} | +5 points")

                await ctx.send(embed=embed)

            except Exception as e:
                logger.error(f"Error in alternatives command: {e}", exc_info=True)
                await ctx.send("❌ An error occurred while fetching alternatives.")
            finally:
                db.close()

    @commands.command(name="care")
    async def care_instructions(self, ctx: commands.Context, *, garment_name: str):
        """
        Get care instructions to extend garment life.

        Usage: !care <garment_name>
        Example: !care wool sweater
        """
        async with ctx.typing():
            db = next(get_db())
            try:
                # Track user
                user = UserService.get_or_create(db, ctx.author.id, ctx.author.name)
                user.add_points(3)
                UserService.update(db, user)

                # Find garment
                garment = GarmentService.get_by_name(db, garment_name)

                if not garment:
                    await ctx.send(f"❌ Garment '{garment_name}' not found.")
                    return

                # Get care tips
                tips = self.analyzer.generate_care_tips(garment)

                # Create embed
                embed = discord.Embed(
                    title=f"🧺 Care Instructions: {garment.name}",
                    description="Follow these tips to extend your garment's life:",
                    color=discord.Color.blue()
                )

                # Add specific care instructions if available
                if garment.care_instructions:
                    embed.add_field(
                        name="Specific Care",
                        value=garment.care_instructions,
                        inline=False
                    )

                # Add general tips
                tips_text = "\n".join(f"• {tip}" for tip in tips[:8])
                embed.add_field(
                    name="General Tips",
                    value=tips_text,
                    inline=False
                )

                # Add washing frequency
                if garment.washing_frequency:
                    embed.add_field(
                        name="Recommended Washing",
                        value=garment.washing_frequency,
                        inline=False
                    )

                embed.set_footer(text=f"Requested by {ctx.author.name} | +3 points")

                await ctx.send(embed=embed)

            except Exception as e:
                logger.error(f"Error in care command: {e}", exc_info=True)
                await ctx.send("❌ An error occurred while fetching care instructions.")
            finally:
                db.close()

    @commands.command(name="tips")
    async def sustainability_tips(self, ctx: commands.Context):
        """
        Get daily sustainability tips.

        Usage: !tips
        """
        async with ctx.typing():
            db = next(get_db())
            try:
                # Track user
                user = UserService.get_or_create(db, ctx.author.id, ctx.author.name)
                user.add_points(2)
                UserService.update(db, user)

                tips = self.analyzer.get_sustainability_tips()

                # Get random selection
                import random
                selected_tips = random.sample(tips, min(5, len(tips)))

                embed = discord.Embed(
                    title="🌍 Sustainability Tips",
                    description="Here are some tips for sustainable fashion:",
                    color=discord.Color.green()
                )

                tips_text = "\n\n".join(f"**{i+1}.** {tip}" for i, tip in enumerate(selected_tips))
                embed.add_field(name="Tips", value=tips_text, inline=False)

                embed.set_footer(text=f"Requested by {ctx.author.name} | +2 points")

                await ctx.send(embed=embed)

            except Exception as e:
                logger.error(f"Error in tips command: {e}", exc_info=True)
                await ctx.send("❌ An error occurred while fetching tips.")
            finally:
                db.close()


async def setup(bot: commands.Bot):
    """Set up the cog."""
    await bot.add_cog(SustainabilityCommands(bot))
