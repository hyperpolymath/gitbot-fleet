"""Sustainability scoring and analysis service."""

from typing import Dict, List, Optional
from models import Material, Garment


class SustainabilityAnalyzer:
    """Analyzer for sustainability metrics and recommendations."""

    @staticmethod
    def get_impact_category(score: float) -> str:
        """
        Get impact category based on score.

        Args:
            score: Sustainability score (0-100)

        Returns:
            Impact category string
        """
        if score >= 80:
            return "Very Low Impact ⭐⭐⭐"
        elif score >= 60:
            return "Low Impact ⭐⭐"
        elif score >= 40:
            return "Medium Impact ⭐"
        else:
            return "High Impact ⚠️"

    @staticmethod
    def get_material_recommendation(material: Material) -> str:
        """
        Get recommendation text for a material.

        Args:
            material: Material instance

        Returns:
            Recommendation string
        """
        score = material.calculate_overall_score()

        if score >= 80:
            return "Excellent choice! This material has minimal environmental impact."
        elif score >= 60:
            return "Good choice! This material is relatively sustainable."
        elif score >= 40:
            return "Consider alternatives. This material has moderate environmental impact."
        else:
            return "Not recommended. This material has significant environmental impact. Look for alternatives."

    @staticmethod
    def generate_care_tips(garment: Garment) -> List[str]:
        """
        Generate care tips to extend garment life.

        Args:
            garment: Garment instance

        Returns:
            List of care tips
        """
        tips = [
            "Wash in cold water to save energy and preserve fabric",
            "Air dry when possible instead of using a tumble dryer",
            "Turn garments inside out before washing to reduce fading",
            "Use eco-friendly detergents to minimize chemical impact",
        ]

        # Add material-specific tips
        if garment.materials:
            for material in garment.materials:
                if "cotton" in material.name.lower():
                    tips.append("Cotton: Iron while slightly damp for best results")
                elif "wool" in material.name.lower():
                    tips.append("Wool: Hand wash or use wool cycle, lay flat to dry")
                elif "synthetic" in material.name.lower() or "polyester" in material.name.lower():
                    tips.append("Synthetic: Avoid high heat to prevent damage and microplastic shedding")

        # Add frequency tips
        tips.append("Don't overwash - air out garments between wears when possible")
        tips.append("Repair minor damage promptly to extend garment life")

        return tips

    @staticmethod
    def calculate_lifetime_impact(garment: Garment) -> Dict[str, str]:
        """
        Calculate lifetime environmental impact of a garment.

        Args:
            garment: Garment instance

        Returns:
            Dictionary with lifetime impact metrics
        """
        impact = garment.get_environmental_impact()

        if garment.expected_lifespan_years and garment.typical_wears:
            # Estimate washing impact
            washes_per_year = garment.typical_wears / 10  # Assume wash every 10 wears
            total_washes = washes_per_year * garment.expected_lifespan_years

            return {
                "production_impact": impact,
                "estimated_washes": f"{int(total_washes)} washes over lifetime",
                "washing_impact": f"~{total_washes * 50:.0f} liters water for washing",
                "total_lifespan": f"{garment.expected_lifespan_years} years",
                "recommendation": f"Wear at least {garment.typical_wears} times to maximize value"
            }

        return {
            "production_impact": impact,
            "recommendation": "Wear as many times as possible to minimize environmental cost per wear"
        }

    @staticmethod
    def compare_materials(material1: Material, material2: Material) -> Dict[str, any]:
        """
        Compare two materials across sustainability metrics.

        Args:
            material1: First material
            material2: Second material

        Returns:
            Comparison dictionary
        """
        return {
            "material1": material1.name,
            "material2": material2.name,
            "overall_scores": {
                material1.name: material1.calculate_overall_score(),
                material2.name: material2.calculate_overall_score()
            },
            "water_usage": {
                material1.name: material1.water_usage_score,
                material2.name: material2.water_usage_score,
                "better": material1.name if material1.water_usage_score > material2.water_usage_score else material2.name
            },
            "carbon_footprint": {
                material1.name: material1.carbon_footprint_score,
                material2.name: material2.carbon_footprint_score,
                "better": material1.name if material1.carbon_footprint_score > material2.carbon_footprint_score else material2.name
            },
            "biodegradability": {
                material1.name: material1.biodegradability_score,
                material2.name: material2.biodegradability_score,
                "better": material1.name if material1.biodegradability_score > material2.biodegradability_score else material2.name
            }
        }

    @staticmethod
    def get_sustainability_tips() -> List[str]:
        """
        Get general sustainability tips.

        Returns:
            List of sustainability tips
        """
        return [
            "Buy less, choose well, make it last - quality over quantity",
            "Support brands with transparent supply chains",
            "Look for certifications like GOTS, Fair Trade, or B Corp",
            "Choose natural or recycled materials when possible",
            "Repair and mend clothes instead of replacing them",
            "Buy second-hand or vintage when you can",
            "Organize clothing swaps with friends",
            "Donate or recycle clothes responsibly",
            "Consider renting formal wear instead of buying",
            "Learn basic sewing skills for repairs and alterations",
            "Store clothes properly to extend their life",
            "Avoid fast fashion and trends that quickly go out of style"
        ]
