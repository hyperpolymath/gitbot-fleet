"""Brand information commands cog."""

import discord
from discord.ext import commands

from models import get_db
from services.database import BrandService, UserService
from utils.logger import get_logger

logger = get_logger(__name__)


class BrandsCommands(commands.Cog):
    """Commands for brand information."""

    def __init__(self, bot: commands.Bot):
        self.bot = bot

    @commands.command(name="brands", aliases=["brand"])
    async def brand_info(self, ctx: commands.Context, *, brand_name: str = None):
        """
        Get information about sustainable brands or search for a specific brand.

        Usage: !brands [brand_name]
        Example: !brands patagonia
        """
        async with ctx.typing():
            db = next(get_db())
            try:
                # Track user
                user = UserService.get_or_create(db, ctx.author.id, ctx.author.name)
                user.add_points(5)
                UserService.update(db, user)

                if brand_name:
                    # Search for specific brand
                    brand = BrandService.get_by_name(db, brand_name)

                    if not brand:
                        await ctx.send(f"❌ Brand '{brand_name}' not found.")
                        return

                    # Create detailed embed
                    embed = discord.Embed(
                        title=f"🏷️ {brand.name}",
                        description=brand.description or "No description available",
                        color=discord.Color.green() if brand.overall_rating >= 60 else discord.Color.orange(),
                        url=brand.website if brand.website else None
                    )

                    embed.add_field(
                        name="Overall Rating",
                        value=f"**{brand.overall_rating:.1f}/100**\n{brand.get_rating_summary()}",
                        inline=False
                    )

                    # Ratings
                    embed.add_field(
                        name="🌍 Environmental",
                        value=f"{brand.environmental_rating:.1f}/100",
                        inline=True
                    )
                    embed.add_field(
                        name="👥 Labor",
                        value=f"{brand.labor_rating:.1f}/100",
                        inline=True
                    )
                    embed.add_field(
                        name="🐾 Animal Welfare",
                        value=f"{brand.animal_welfare_rating:.1f}/100",
                        inline=True
                    )

                    # Certifications
                    badges = brand.get_certification_badges()
                    if badges:
                        embed.add_field(
                            name="Certifications",
                            value="\n".join(f"✓ {badge}" for badge in badges),
                            inline=False
                        )

                    # Additional info
                    if brand.country:
                        embed.add_field(name="Country", value=brand.country, inline=True)
                    if brand.price_range:
                        embed.add_field(name="Price Range", value=brand.price_range, inline=True)
                    if brand.transparency_score:
                        embed.add_field(
                            name="Transparency",
                            value=f"{brand.transparency_score:.1f}/100",
                            inline=True
                        )

                    if brand.good_on_you_rating:
                        embed.add_field(
                            name="Good On You Rating",
                            value=brand.good_on_you_rating,
                            inline=False
                        )

                    embed.set_footer(text=f"Requested by {ctx.author.name} | +5 points")

                    await ctx.send(embed=embed)

                else:
                    # Show top-rated brands
                    brands = BrandService.get_top_rated(db, limit=10)

                    if not brands:
                        await ctx.send("❌ No brands found in database.")
                        return

                    embed = discord.Embed(
                        title="🌟 Top Sustainable Brands",
                        description="Here are the highest-rated sustainable fashion brands:",
                        color=discord.Color.gold()
                    )

                    for i, brand in enumerate(brands, 1):
                        embed.add_field(
                            name=f"{i}. {brand.name} - {brand.overall_rating:.1f}/100",
                            value=brand.get_rating_summary(),
                            inline=False
                        )

                    embed.set_footer(text=f"Use !brands <name> for detailed info | +5 points")

                    await ctx.send(embed=embed)

            except Exception as e:
                logger.error(f"Error in brands command: {e}", exc_info=True)
                await ctx.send("❌ An error occurred while fetching brand data.")
            finally:
                db.close()


async def setup(bot: commands.Bot):
    """Set up the cog."""
    await bot.add_cog(BrandsCommands(bot))
