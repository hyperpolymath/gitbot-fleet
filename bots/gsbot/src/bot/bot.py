"""Discord bot implementation for Garment Sustainability Bot."""

import discord
from discord.ext import commands
from typing import Optional

from config.settings import config
from utils.logger import setup_logger, get_logger
from models import init_db

# Set up logger
logger = setup_logger("gsbot", config.LOG_LEVEL, config.LOG_FILE)


class GarmentSustainabilityBot(commands.Bot):
    """Main bot class for Garment Sustainability Bot."""

    def __init__(self):
        """Initialize the bot."""
        intents = discord.Intents.default()
        intents.message_content = True
        intents.members = True

        super().__init__(
            command_prefix=config.DISCORD_PREFIX,
            intents=intents,
            help_command=commands.DefaultHelpCommand(),
        )

        self.logger = get_logger("gsbot")

    async def setup_hook(self):
        """Set up the bot before it starts."""
        self.logger.info("Setting up Garment Sustainability Bot...")

        # Initialize database
        try:
            init_db()
            self.logger.info("Database initialized successfully")
        except Exception as e:
            self.logger.error(f"Failed to initialize database: {e}")
            raise

        # Load cogs (command groups)
        await self.load_cogs()

        self.logger.info("Bot setup complete")

    async def load_cogs(self):
        """Load all cog modules."""
        cogs = [
            "bot.cogs.sustainability",
            "bot.cogs.materials",
            "bot.cogs.brands",
            "bot.cogs.user_commands",
            "bot.cogs.admin",
        ]

        for cog in cogs:
            try:
                await self.load_extension(cog)
                self.logger.info(f"Loaded cog: {cog}")
            except Exception as e:
                self.logger.error(f"Failed to load cog {cog}: {e}")

    async def on_ready(self):
        """Called when the bot is ready."""
        self.logger.info(f"Logged in as {self.user.name} ({self.user.id})")
        self.logger.info(f"Connected to {len(self.guilds)} guilds")

        # Set bot status
        await self.change_presence(
            activity=discord.Activity(
                type=discord.ActivityType.watching,
                name=f"sustainable fashion | {config.DISCORD_PREFIX}help"
            )
        )

    async def on_command_error(self, ctx: commands.Context, error: Exception):
        """Handle command errors."""
        if isinstance(error, commands.CommandNotFound):
            await ctx.send(
                f"❌ Command not found. Use `{config.DISCORD_PREFIX}help` to see available commands."
            )
        elif isinstance(error, commands.MissingRequiredArgument):
            await ctx.send(f"❌ Missing required argument: {error.param.name}")
        elif isinstance(error, commands.MissingPermissions):
            await ctx.send("❌ You don't have permission to use this command.")
        else:
            self.logger.error(f"Command error: {error}", exc_info=True)
            await ctx.send("❌ An error occurred while processing the command.")

    async def on_message(self, message: discord.Message):
        """Process messages."""
        # Ignore messages from the bot itself
        if message.author == self.user:
            return

        # Process commands
        await self.process_commands(message)


def create_bot() -> GarmentSustainabilityBot:
    """
    Create and return a bot instance.

    Returns:
        GarmentSustainabilityBot instance
    """
    return GarmentSustainabilityBot()
