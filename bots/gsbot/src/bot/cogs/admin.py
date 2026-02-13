"""Admin commands cog."""

import discord
from discord.ext import commands

from config.settings import config
from models import get_db
from utils.logger import get_logger

logger = get_logger(__name__)


def is_admin():
    """Check if user is an admin."""
    async def predicate(ctx: commands.Context):
        return ctx.author.id in config.DISCORD_ADMIN_IDS or ctx.author.guild_permissions.administrator
    return commands.check(predicate)


class AdminCommands(commands.Cog):
    """Admin-only commands."""

    def __init__(self, bot: commands.Bot):
        self.bot = bot

    @commands.command(name="loaddata")
    @is_admin()
    async def load_data(self, ctx: commands.Context):
        """
        Load sample data into the database.

        Usage: !loaddata
        """
        async with ctx.typing():
            try:
                from scripts.load_fixtures import load_all_fixtures

                await ctx.send("⏳ Loading data fixtures...")

                db = next(get_db())
                try:
                    counts = load_all_fixtures(db)

                    embed = discord.Embed(
                        title="✅ Data Loaded Successfully",
                        color=discord.Color.green()
                    )

                    embed.add_field(name="Materials", value=str(counts.get("materials", 0)), inline=True)
                    embed.add_field(name="Garments", value=str(counts.get("garments", 0)), inline=True)
                    embed.add_field(name="Brands", value=str(counts.get("brands", 0)), inline=True)

                    await ctx.send(embed=embed)

                finally:
                    db.close()

            except Exception as e:
                logger.error(f"Error loading data: {e}", exc_info=True)
                await ctx.send(f"❌ Error loading data: {str(e)}")

    @commands.command(name="stats")
    @is_admin()
    async def bot_stats(self, ctx: commands.Context):
        """
        Show bot statistics.

        Usage: !stats
        """
        async with ctx.typing():
            db = next(get_db())
            try:
                from models import Material, Garment, Brand, User

                material_count = db.query(Material).count()
                garment_count = db.query(Garment).count()
                brand_count = db.query(Brand).count()
                user_count = db.query(User).count()

                embed = discord.Embed(
                    title="📊 Bot Statistics",
                    color=discord.Color.blue()
                )

                embed.add_field(name="Guilds", value=str(len(self.bot.guilds)), inline=True)
                embed.add_field(name="Users Tracked", value=str(user_count), inline=True)
                embed.add_field(name="Latency", value=f"{self.bot.latency*1000:.0f}ms", inline=True)

                embed.add_field(name="Materials", value=str(material_count), inline=True)
                embed.add_field(name="Garments", value=str(garment_count), inline=True)
                embed.add_field(name="Brands", value=str(brand_count), inline=True)

                await ctx.send(embed=embed)

            except Exception as e:
                logger.error(f"Error getting stats: {e}", exc_info=True)
                await ctx.send("❌ An error occurred while fetching stats.")
            finally:
                db.close()

    @commands.command(name="announce")
    @is_admin()
    async def announce(self, ctx: commands.Context, *, message: str):
        """
        Send an announcement to all guilds.

        Usage: !announce <message>
        """
        try:
            sent_count = 0
            failed_count = 0

            for guild in self.bot.guilds:
                try:
                    # Try to find a general channel
                    channel = discord.utils.get(guild.text_channels, name="general")
                    if not channel:
                        channel = guild.text_channels[0]  # First available channel

                    embed = discord.Embed(
                        title="📢 Announcement",
                        description=message,
                        color=discord.Color.blue()
                    )
                    embed.set_footer(text="Garment Sustainability Bot")

                    await channel.send(embed=embed)
                    sent_count += 1

                except Exception as e:
                    logger.error(f"Failed to send announcement to {guild.name}: {e}")
                    failed_count += 1

            await ctx.send(
                f"✅ Announcement sent to {sent_count} guild(s). Failed: {failed_count}"
            )

        except Exception as e:
            logger.error(f"Error in announce command: {e}", exc_info=True)
            await ctx.send("❌ An error occurred while sending announcement.")


async def setup(bot: commands.Bot):
    """Set up the cog."""
    await bot.add_cog(AdminCommands(bot))
