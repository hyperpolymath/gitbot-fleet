"""Main entry point for the Garment Sustainability Bot."""

import asyncio
import sys
from pathlib import Path

# Add src directory to path
sys.path.insert(0, str(Path(__file__).parent.parent))

from bot.bot import create_bot
from config.settings import config
from utils.logger import get_logger

logger = get_logger("gsbot.main")


async def main():
    """Main function to run the bot."""
    try:
        # Validate configuration
        config.validate()
        logger.info("Configuration validated successfully")

        # Create and run bot
        bot = create_bot()
        async with bot:
            logger.info("Starting Garment Sustainability Bot...")
            await bot.start(config.DISCORD_TOKEN)

    except ValueError as e:
        logger.error(f"Configuration error: {e}")
        sys.exit(1)
    except Exception as e:
        logger.error(f"Failed to start bot: {e}", exc_info=True)
        sys.exit(1)


if __name__ == "__main__":
    try:
        asyncio.run(main())
    except KeyboardInterrupt:
        logger.info("Bot stopped by user")
    except Exception as e:
        logger.error(f"Unexpected error: {e}", exc_info=True)
        sys.exit(1)
