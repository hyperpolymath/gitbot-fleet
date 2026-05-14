"""Configuration settings for the Garment Sustainability Bot."""

import os
from pathlib import Path
from typing import Optional
from dotenv import load_dotenv

# Load environment variables from .env file
load_dotenv()


class Config:
    """Application configuration class."""

    # Base directory
    BASE_DIR = Path(__file__).resolve().parent.parent.parent

    # Discord Configuration
    DISCORD_TOKEN: str = os.getenv("DISCORD_TOKEN", "")
    DISCORD_PREFIX: str = os.getenv("DISCORD_PREFIX", "!")
    DISCORD_ADMIN_IDS: list[int] = [
        int(id_str) for id_str in os.getenv("DISCORD_ADMIN_IDS", "").split(",") if id_str
    ]

    # Database Configuration
    DATABASE_URL: str = os.getenv(
        "DATABASE_URL", f"sqlite:///{BASE_DIR}/data/gsbot.db"
    )
    DATABASE_ECHO: bool = os.getenv("DATABASE_ECHO", "False").lower() == "true"

    # Cache Configuration
    CACHE_TTL: int = int(os.getenv("CACHE_TTL", "3600"))  # 1 hour default
    CACHE_MAXSIZE: int = int(os.getenv("CACHE_MAXSIZE", "1000"))

    # Logging Configuration
    LOG_LEVEL: str = os.getenv("LOG_LEVEL", "INFO")
    LOG_FILE: Optional[str] = os.getenv("LOG_FILE", None)

    # API Configuration (for future external integrations)
    API_TIMEOUT: int = int(os.getenv("API_TIMEOUT", "30"))
    API_RETRY_COUNT: int = int(os.getenv("API_RETRY_COUNT", "3"))

    # Feature Flags
    ENABLE_CACHING: bool = os.getenv("ENABLE_CACHING", "True").lower() == "true"
    ENABLE_ANALYTICS: bool = os.getenv("ENABLE_ANALYTICS", "False").lower() == "true"

    # Data Directory
    DATA_DIR: Path = BASE_DIR / "data"
    FIXTURES_DIR: Path = DATA_DIR / "fixtures"

    @classmethod
    def validate(cls) -> None:
        """Validate required configuration values."""
        if not cls.DISCORD_TOKEN:
            raise ValueError("DISCORD_TOKEN environment variable is required")

        # Ensure data directories exist
        cls.DATA_DIR.mkdir(parents=True, exist_ok=True)
        cls.FIXTURES_DIR.mkdir(parents=True, exist_ok=True)

    @classmethod
    def get_database_path(cls) -> Path:
        """Get the database file path."""
        if cls.DATABASE_URL.startswith("sqlite:///"):
            db_path = cls.DATABASE_URL.replace("sqlite:///", "")
            return Path(db_path)
        raise ValueError("Only SQLite databases are supported in this version")


# Singleton instance
config = Config()
