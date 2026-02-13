"""User-related commands cog."""

import discord
from discord.ext import commands

from models import get_db
from services.database import UserService
from utils.logger import get_logger

logger = get_logger(__name__)


class UserCommands(commands.Cog):
    """Commands for user tracking and gamification."""

    def __init__(self, bot: commands.Bot):
        self.bot = bot

    @commands.command(name="profile", aliases=["stats", "me"])
    async def profile(self, ctx: commands.Context):
        """
        View your sustainability profile.

        Usage: !profile
        """
        async with ctx.typing():
            db = next(get_db())
            try:
                user = UserService.get_or_create(db, ctx.author.id, ctx.author.name)

                embed = discord.Embed(
                    title=f"👤 {user.username}'s Profile",
                    color=discord.Color.blue()
                )

                embed.add_field(
                    name="🏆 Rank",
                    value=user.get_rank(),
                    inline=True
                )
                embed.add_field(
                    name="📊 Level",
                    value=str(user.level),
                    inline=True
                )
                embed.add_field(
                    name="⭐ Points",
                    value=str(user.sustainability_points),
                    inline=True
                )
                embed.add_field(
                    name="📝 Queries",
                    value=str(user.queries_count),
                    inline=True
                )

                # Progress to next level
                points_to_next = ((user.level) * 100) - user.sustainability_points
                if points_to_next > 0:
                    embed.add_field(
                        name="📈 Progress",
                        value=f"{points_to_next} points to level {user.level + 1}",
                        inline=True
                    )

                # Preferences
                if user.preferred_materials:
                    embed.add_field(
                        name="Preferred Materials",
                        value=user.preferred_materials,
                        inline=False
                    )

                if user.sustainability_priority:
                    embed.add_field(
                        name="Priority",
                        value=user.sustainability_priority.replace("_", " ").title(),
                        inline=True
                    )

                if user.budget_range:
                    embed.add_field(
                        name="Budget Range",
                        value=user.budget_range,
                        inline=True
                    )

                embed.set_thumbnail(url=ctx.author.display_avatar.url)
                embed.set_footer(text="Keep learning about sustainability to earn more points!")

                await ctx.send(embed=embed)

            except Exception as e:
                logger.error(f"Error in profile command: {e}", exc_info=True)
                await ctx.send("❌ An error occurred while fetching your profile.")
            finally:
                db.close()

    @commands.command(name="leaderboard", aliases=["lb", "top"])
    async def leaderboard(self, ctx: commands.Context):
        """
        View the sustainability leaderboard.

        Usage: !leaderboard
        """
        async with ctx.typing():
            db = next(get_db())
            try:
                top_users = UserService.get_leaderboard(db, limit=10)

                if not top_users:
                    await ctx.send("❌ No users found on leaderboard.")
                    return

                embed = discord.Embed(
                    title="🏆 Sustainability Champions Leaderboard",
                    description="Top users making a difference:",
                    color=discord.Color.gold()
                )

                medals = ["🥇", "🥈", "🥉"]

                for i, user in enumerate(top_users, 1):
                    medal = medals[i-1] if i <= 3 else f"**{i}.**"
                    embed.add_field(
                        name=f"{medal} {user.username}",
                        value=f"Level {user.level} • {user.sustainability_points} points • {user.get_rank()}",
                        inline=False
                    )

                # Show current user's rank if not in top 10
                current_user = UserService.get_by_discord_id(db, ctx.author.id)
                if current_user and current_user not in top_users:
                    embed.add_field(
                        name=f"\nYour Stats",
                        value=f"Level {current_user.level} • {current_user.sustainability_points} points",
                        inline=False
                    )

                embed.set_footer(text="Use !profile to view your complete profile")

                await ctx.send(embed=embed)

            except Exception as e:
                logger.error(f"Error in leaderboard command: {e}", exc_info=True)
                await ctx.send("❌ An error occurred while fetching leaderboard.")
            finally:
                db.close()

    @commands.command(name="setpreference", aliases=["pref"])
    async def set_preference(
        self, ctx: commands.Context, preference_type: str, *, value: str
    ):
        """
        Set your sustainability preferences.

        Usage: !setpreference <type> <value>
        Types: materials, budget, priority
        Example: !setpreference materials organic cotton, linen
        """
        db = next(get_db())
        try:
            user = UserService.get_or_create(db, ctx.author.id, ctx.author.name)

            preference_type = preference_type.lower()

            if preference_type in ["materials", "material"]:
                user.preferred_materials = value
                await ctx.send(f"✅ Preferred materials set to: {value}")
            elif preference_type == "budget":
                if value not in ["$", "$$", "$$$", "$$$$"]:
                    await ctx.send("❌ Budget must be: $, $$, $$$, or $$$$")
                    return
                user.budget_range = value
                await ctx.send(f"✅ Budget range set to: {value}")
            elif preference_type == "priority":
                valid_priorities = ["environmental", "social", "animal_welfare", "all"]
                if value.lower() not in valid_priorities:
                    await ctx.send(f"❌ Priority must be one of: {', '.join(valid_priorities)}")
                    return
                user.sustainability_priority = value.lower()
                await ctx.send(f"✅ Sustainability priority set to: {value}")
            else:
                await ctx.send(
                    "❌ Invalid preference type. Use: materials, budget, or priority"
                )
                return

            UserService.update(db, user)

        except Exception as e:
            logger.error(f"Error in setpreference command: {e}", exc_info=True)
            await ctx.send("❌ An error occurred while setting preference.")
        finally:
            db.close()


async def setup(bot: commands.Bot):
    """Set up the cog."""
    await bot.add_cog(UserCommands(bot))
