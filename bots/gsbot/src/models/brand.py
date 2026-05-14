"""Brand model for tracking sustainable fashion brands."""

from sqlalchemy import Column, String, Text, Float, Boolean

from models.base import BaseModel


class Brand(BaseModel):
    """Brand model representing fashion brands and their sustainability ratings."""

    __tablename__ = "brands"

    name = Column(String(100), unique=True, nullable=False, index=True)
    description = Column(Text)
    website = Column(String(255))

    # Sustainability ratings (0-100)
    overall_rating = Column(Float, default=50.0)
    environmental_rating = Column(Float, default=50.0)
    labor_rating = Column(Float, default=50.0)
    animal_welfare_rating = Column(Float, default=50.0)

    # Certifications and practices
    is_certified_bcorp = Column(Boolean, default=False)
    is_fair_trade = Column(Boolean, default=False)
    is_organic_certified = Column(Boolean, default=False)
    uses_recycled_materials = Column(Boolean, default=False)
    carbon_neutral = Column(Boolean, default=False)

    # Additional information
    country = Column(String(100))
    price_range = Column(String(20))  # e.g., "$", "$$", "$$$", "$$$$"
    transparency_score = Column(Float)  # 0-100

    # Good On You rating equivalent
    good_on_you_rating = Column(String(20))  # "Great", "Good", "It's a start", "Not good enough", "We avoid"

    def get_certification_badges(self) -> list[str]:
        """
        Get list of certification badges.

        Returns:
            List of certification names
        """
        badges = []
        if self.is_certified_bcorp:
            badges.append("B Corp Certified")
        if self.is_fair_trade:
            badges.append("Fair Trade")
        if self.is_organic_certified:
            badges.append("Organic Certified")
        if self.uses_recycled_materials:
            badges.append("Uses Recycled Materials")
        if self.carbon_neutral:
            badges.append("Carbon Neutral")
        return badges

    def get_rating_summary(self) -> str:
        """
        Get human-readable rating summary.

        Returns:
            Rating summary string
        """
        score = self.overall_rating
        if score >= 80:
            return "Excellent sustainability practices"
        elif score >= 60:
            return "Good sustainability efforts"
        elif score >= 40:
            return "Making progress on sustainability"
        else:
            return "Needs significant improvement"

    def __repr__(self) -> str:
        return f"<Brand(name='{self.name}', rating={self.overall_rating:.1f})>"
