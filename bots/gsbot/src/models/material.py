"""Material model for tracking garment materials and their sustainability metrics."""

from sqlalchemy import Column, String, Float, Text, Enum
import enum

from models.base import BaseModel


class MaterialType(enum.Enum):
    """Types of materials."""
    NATURAL = "natural"
    SYNTHETIC = "synthetic"
    SEMI_SYNTHETIC = "semi_synthetic"
    RECYCLED = "recycled"
    ORGANIC = "organic"


class Material(BaseModel):
    """Material model representing different fabric materials."""

    __tablename__ = "materials"

    name = Column(String(100), unique=True, nullable=False, index=True)
    material_type = Column(Enum(MaterialType), nullable=False)
    description = Column(Text)

    # Environmental Impact Scores (0-100, higher is better)
    water_usage_score = Column(Float, default=50.0)  # Water efficiency
    carbon_footprint_score = Column(Float, default=50.0)  # Carbon emissions
    biodegradability_score = Column(Float, default=50.0)  # End-of-life impact
    chemical_usage_score = Column(Float, default=50.0)  # Chemical processing
    energy_consumption_score = Column(Float, default=50.0)  # Energy usage

    # Production metrics
    water_liters_per_kg = Column(Float)  # Liters of water per kg of material
    co2_kg_per_kg = Column(Float)  # kg of CO2 per kg of material
    energy_mj_per_kg = Column(Float)  # MJ of energy per kg of material

    # Additional properties
    is_biodegradable = Column(String(50))  # e.g., "Yes", "No", "Partially"
    recycling_potential = Column(String(50))  # e.g., "High", "Medium", "Low"
    durability_rating = Column(Float)  # 0-10 scale

    # Care instructions impact
    typical_wash_temp = Column(String(20))  # e.g., "cold", "warm", "hot"
    drying_method = Column(String(50))  # e.g., "air dry", "tumble dry low"

    def calculate_overall_score(self) -> float:
        """
        Calculate overall sustainability score.

        Returns:
            Overall sustainability score (0-100)
        """
        scores = [
            self.water_usage_score,
            self.carbon_footprint_score,
            self.biodegradability_score,
            self.chemical_usage_score,
            self.energy_consumption_score,
        ]
        return sum(scores) / len(scores)

    def get_grade(self) -> str:
        """
        Get letter grade for sustainability.

        Returns:
            Letter grade (A+ to F)
        """
        score = self.calculate_overall_score()
        if score >= 90:
            return "A+"
        elif score >= 80:
            return "A"
        elif score >= 70:
            return "B"
        elif score >= 60:
            return "C"
        elif score >= 50:
            return "D"
        else:
            return "F"

    def __repr__(self) -> str:
        return f"<Material(name='{self.name}', type='{self.material_type.value}', score={self.calculate_overall_score():.1f})>"
