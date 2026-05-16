"""Script to export database data to JSON."""

import sys
import json
from pathlib import Path
from datetime import datetime

# Add src to path
sys.path.insert(0, str(Path(__file__).parent.parent / "src"))

from models import get_db, Material, Garment, Brand, User
from utils.logger import setup_logger, get_logger

logger = setup_logger("export", "INFO")


def export_materials(db_session) -> list:
    """Export all materials to dict format."""
    materials = db_session.query(Material).all()
    return [
        {
            "name": m.name,
            "type": m.material_type.value,
            "description": m.description,
            "scores": {
                "water_usage": m.water_usage_score,
                "carbon_footprint": m.carbon_footprint_score,
                "biodegradability": m.biodegradability_score,
                "chemical_usage": m.chemical_usage_score,
                "energy_consumption": m.energy_consumption_score,
            },
            "metrics": {
                "water_liters_per_kg": m.water_liters_per_kg,
                "co2_kg_per_kg": m.co2_kg_per_kg,
                "energy_mj_per_kg": m.energy_mj_per_kg,
            }
        }
        for m in materials
    ]


def export_garments(db_session) -> list:
    """Export all garments to dict format."""
    garments = db_session.query(Garment).all()
    return [
        {
            "name": g.name,
            "category": g.category,
            "description": g.description,
            "sustainability_score": g.sustainability_score,
            "materials": [m.name for m in g.materials],
        }
        for g in garments
    ]


def export_brands(db_session) -> list:
    """Export all brands to dict format."""
    brands = db_session.query(Brand).all()
    return [
        {
            "name": b.name,
            "description": b.description,
            "website": b.website,
            "ratings": {
                "overall": b.overall_rating,
                "environmental": b.environmental_rating,
                "labor": b.labor_rating,
                "animal_welfare": b.animal_welfare_rating,
            },
            "certifications": b.get_certification_badges(),
        }
        for b in brands
    ]


def export_all_data():
    """Export all database data to JSON files."""
    output_dir = Path(__file__).parent.parent / "data" / "exports"
    output_dir.mkdir(parents=True, exist_ok=True)

    timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")

    db = next(get_db())
    try:
        # Export each data type
        exports = {
            "materials": export_materials(db),
            "garments": export_garments(db),
            "brands": export_brands(db),
        }

        for data_type, data in exports.items():
            filename = output_dir / f"{data_type}_{timestamp}.json"
            with open(filename, 'w') as f:
                json.dump(data, f, indent=2)
            logger.info(f"Exported {len(data)} {data_type} to {filename}")

        # Export combined file
        combined_file = output_dir / f"all_data_{timestamp}.json"
        with open(combined_file, 'w') as f:
            json.dump(exports, f, indent=2)
        logger.info(f"Exported combined data to {combined_file}")

        print(f"\n✅ Export completed successfully!")
        print(f"   Materials: {len(exports['materials'])}")
        print(f"   Garments: {len(exports['garments'])}")
        print(f"   Brands: {len(exports['brands'])}")
        print(f"\nFiles saved to: {output_dir}")

    finally:
        db.close()


if __name__ == "__main__":
    export_all_data()
