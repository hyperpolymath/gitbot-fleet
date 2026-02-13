"""User model for tracking user preferences and interactions."""

from sqlalchemy import Column, String, BigInteger, Integer, Boolean

from models.base import BaseModel


class User(BaseModel):
    """User model for Discord bot users."""

    __tablename__ = "users"

    discord_id = Column(BigInteger, unique=True, nullable=False, index=True)
    username = Column(String(100))

    # User preferences
    preferred_materials = Column(String(500))  # Comma-separated list
    budget_range = Column(String(20))  # "$", "$$", "$$$", "$$$$"
    sustainability_priority = Column(String(50))  # "environmental", "social", "animal_welfare", "all"

    # Tracking and gamification
    queries_count = Column(Integer, default=0)
    sustainability_points = Column(Integer, default=0)
    level = Column(Integer, default=1)

    # Notifications
    daily_tips_enabled = Column(Boolean, default=True)

    def add_points(self, points: int) -> None:
        """
        Add sustainability points and potentially level up.

        Args:
            points: Points to add
        """
        self.sustainability_points += points
        self.queries_count += 1

        # Simple leveling system: 100 points per level
        new_level = (self.sustainability_points // 100) + 1
        if new_level > self.level:
            self.level = new_level

    def get_rank(self) -> str:
        """
        Get user rank based on level.

        Returns:
            Rank name
        """
        if self.level >= 20:
            return "Sustainability Champion"
        elif self.level >= 15:
            return "Eco Warrior"
        elif self.level >= 10:
            return "Green Enthusiast"
        elif self.level >= 5:
            return "Conscious Consumer"
        else:
            return "Sustainability Learner"

    def __repr__(self) -> str:
        return f"<User(discord_id={self.discord_id}, username='{self.username}', level={self.level})>"
