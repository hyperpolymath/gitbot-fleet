"""Garment model for tracking different types of clothing items."""

from sqlalchemy import Column, String, Text, Float, Integer, ForeignKey, Table
from sqlalchemy.orm import relationship

from models.base import BaseModel, Base


# Association table for many-to-many relationship between garments and materials
garment_materials = Table(
    'garment_materials',
    Base.metadata,
    Column('garment_id', Integer, ForeignKey('garments.id'), primary_key=True),
    Column('material_id', Integer, ForeignKey('materials.id'), primary_key=True),
    Column('percentage', Float, default=100.0)  # Percentage of this material in garment
)


class Garment(BaseModel):
    """Garment model representing different types of clothing."""

    __tablename__ = "garments"

    name = Column(String(100), unique=True, nullable=False, index=True)
    category = Column(String(50), nullable=False, index=True)  # e.g., "shirt", "pants", "dress"
    description = Column(Text)

    # Typical garment properties
    typical_weight_kg = Column(Float)  # Average weight in kg
    expected_lifespan_years = Column(Float)  # Expected lifespan in years
    typical_wears = Column(Integer)  # Average number of wears before disposal

    # Care instructions
    care_instructions = Column(Text)
    washing_frequency = Column(String(50))  # e.g., "after each wear", "after 2-3 wears"

    # Relationships
    materials = relationship("Material", secondary=garment_materials, backref="garments")

    # Calculated sustainability metrics
    sustainability_score = Column(Float)  # Overall score (0-100)

    def calculate_sustainability_score(self) -> float:
        """
        Calculate overall sustainability score based on materials.

        Returns:
            Sustainability score (0-100)
        """
        if not self.materials:
            return 50.0  # Default neutral score

        total_score = sum(material.calculate_overall_score() for material in self.materials)
        base_score = total_score / len(self.materials)

        # Adjust for lifespan (longer lifespan = better)
        lifespan_multiplier = 1.0
        if self.expected_lifespan_years:
            if self.expected_lifespan_years >= 5:
                lifespan_multiplier = 1.2
            elif self.expected_lifespan_years >= 3:
                lifespan_multiplier = 1.1
            elif self.expected_lifespan_years < 1:
                lifespan_multiplier = 0.8

        final_score = min(base_score * lifespan_multiplier, 100.0)
        return final_score

    def get_environmental_impact(self) -> dict:
        """
        Get environmental impact summary.

        Returns:
            Dictionary with impact metrics
        """
        if not self.materials:
            return {
                "water_usage": "Unknown",
                "carbon_footprint": "Unknown",
                "energy_consumption": "Unknown"
            }

        total_water = 0.0
        total_co2 = 0.0
        total_energy = 0.0
        count = 0

        for material in self.materials:
            if material.water_liters_per_kg and self.typical_weight_kg:
                total_water += material.water_liters_per_kg * self.typical_weight_kg
            if material.co2_kg_per_kg and self.typical_weight_kg:
                total_co2 += material.co2_kg_per_kg * self.typical_weight_kg
            if material.energy_mj_per_kg and self.typical_weight_kg:
                total_energy += material.energy_mj_per_kg * self.typical_weight_kg
            count += 1

        if count == 0:
            return {
                "water_usage": "Unknown",
                "carbon_footprint": "Unknown",
                "energy_consumption": "Unknown"
            }

        return {
            "water_usage": f"{total_water / count:.1f} liters",
            "carbon_footprint": f"{total_co2 / count:.2f} kg CO2",
            "energy_consumption": f"{total_energy / count:.1f} MJ"
        }

    def __repr__(self) -> str:
        return f"<Garment(name='{self.name}', category='{self.category}')>"
