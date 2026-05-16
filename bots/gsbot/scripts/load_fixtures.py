"""Script to load sample data fixtures into the database."""

import sys
from pathlib import Path

# Add src to path
sys.path.insert(0, str(Path(__file__).parent.parent / "src"))

from sqlalchemy.orm import Session
from models import Material, MaterialType, Garment, Brand, init_db
from utils.logger import get_logger

logger = get_logger(__name__)


def load_materials(db: Session) -> int:
    """Load material fixtures."""
    materials_data = [
        {
            "name": "Organic Cotton",
            "material_type": MaterialType.ORGANIC,
            "description": "Cotton grown without synthetic pesticides or fertilizers, better for soil and water",
            "water_usage_score": 65.0,
            "carbon_footprint_score": 75.0,
            "biodegradability_score": 95.0,
            "chemical_usage_score": 90.0,
            "energy_consumption_score": 70.0,
            "water_liters_per_kg": 10000.0,
            "co2_kg_per_kg": 1.8,
            "energy_mj_per_kg": 55.0,
            "is_biodegradable": "Yes",
            "recycling_potential": "Medium",
            "durability_rating": 7.0,
            "typical_wash_temp": "cold",
            "drying_method": "air dry recommended"
        },
        {
            "name": "Conventional Cotton",
            "material_type": MaterialType.NATURAL,
            "description": "Traditional cotton with high water and pesticide use",
            "water_usage_score": 30.0,
            "carbon_footprint_score": 40.0,
            "biodegradability_score": 90.0,
            "chemical_usage_score": 25.0,
            "energy_consumption_score": 45.0,
            "water_liters_per_kg": 20000.0,
            "co2_kg_per_kg": 5.3,
            "energy_mj_per_kg": 85.0,
            "is_biodegradable": "Yes",
            "recycling_potential": "Medium",
            "durability_rating": 6.5,
            "typical_wash_temp": "warm",
            "drying_method": "tumble dry acceptable"
        },
        {
            "name": "Polyester",
            "material_type": MaterialType.SYNTHETIC,
            "description": "Petroleum-based synthetic fiber, durable but not biodegradable",
            "water_usage_score": 85.0,
            "carbon_footprint_score": 30.0,
            "biodegradability_score": 10.0,
            "chemical_usage_score": 35.0,
            "energy_consumption_score": 25.0,
            "water_liters_per_kg": 70.0,
            "co2_kg_per_kg": 7.0,
            "energy_mj_per_kg": 125.0,
            "is_biodegradable": "No",
            "recycling_potential": "Low",
            "durability_rating": 8.5,
            "typical_wash_temp": "cold",
            "drying_method": "low heat only"
        },
        {
            "name": "Recycled Polyester",
            "material_type": MaterialType.RECYCLED,
            "description": "Polyester made from recycled plastic bottles and textile waste",
            "water_usage_score": 90.0,
            "carbon_footprint_score": 70.0,
            "biodegradability_score": 10.0,
            "chemical_usage_score": 60.0,
            "energy_consumption_score": 75.0,
            "water_liters_per_kg": 30.0,
            "co2_kg_per_kg": 3.0,
            "energy_mj_per_kg": 60.0,
            "is_biodegradable": "No",
            "recycling_potential": "Medium",
            "durability_rating": 8.5,
            "typical_wash_temp": "cold",
            "drying_method": "air dry preferred"
        },
        {
            "name": "Linen",
            "material_type": MaterialType.NATURAL,
            "description": "Made from flax plant, highly sustainable with minimal water needs",
            "water_usage_score": 95.0,
            "carbon_footprint_score": 85.0,
            "biodegradability_score": 100.0,
            "chemical_usage_score": 85.0,
            "energy_consumption_score": 80.0,
            "water_liters_per_kg": 2500.0,
            "co2_kg_per_kg": 0.5,
            "energy_mj_per_kg": 30.0,
            "is_biodegradable": "Yes",
            "recycling_potential": "High",
            "durability_rating": 9.0,
            "typical_wash_temp": "cold",
            "drying_method": "air dry"
        },
        {
            "name": "Hemp",
            "material_type": MaterialType.NATURAL,
            "description": "Fast-growing plant requiring minimal water and no pesticides",
            "water_usage_score": 98.0,
            "carbon_footprint_score": 90.0,
            "biodegradability_score": 100.0,
            "chemical_usage_score": 95.0,
            "energy_consumption_score": 85.0,
            "water_liters_per_kg": 2100.0,
            "co2_kg_per_kg": 0.3,
            "energy_mj_per_kg": 25.0,
            "is_biodegradable": "Yes",
            "recycling_potential": "High",
            "durability_rating": 9.5,
            "typical_wash_temp": "cold",
            "drying_method": "air dry"
        },
        {
            "name": "Wool",
            "material_type": MaterialType.NATURAL,
            "description": "Natural fiber from sheep, biodegradable but with animal welfare concerns",
            "water_usage_score": 55.0,
            "carbon_footprint_score": 45.0,
            "biodegradability_score": 95.0,
            "chemical_usage_score": 50.0,
            "energy_consumption_score": 50.0,
            "water_liters_per_kg": 125000.0,
            "co2_kg_per_kg": 10.5,
            "energy_mj_per_kg": 65.0,
            "is_biodegradable": "Yes",
            "recycling_potential": "Medium",
            "durability_rating": 8.0,
            "typical_wash_temp": "cold",
            "drying_method": "lay flat to dry"
        },
        {
            "name": "Tencel (Lyocell)",
            "material_type": MaterialType.SEMI_SYNTHETIC,
            "description": "Made from wood pulp using closed-loop process, very sustainable",
            "water_usage_score": 90.0,
            "carbon_footprint_score": 85.0,
            "biodegradability_score": 90.0,
            "chemical_usage_score": 85.0,
            "energy_consumption_score": 80.0,
            "water_liters_per_kg": 1000.0,
            "co2_kg_per_kg": 1.2,
            "energy_mj_per_kg": 45.0,
            "is_biodegradable": "Yes",
            "recycling_potential": "High",
            "durability_rating": 7.5,
            "typical_wash_temp": "cold",
            "drying_method": "air dry"
        },
        {
            "name": "Bamboo Viscose",
            "material_type": MaterialType.SEMI_SYNTHETIC,
            "description": "Made from bamboo but chemical-intensive processing reduces sustainability",
            "water_usage_score": 70.0,
            "carbon_footprint_score": 65.0,
            "biodegradability_score": 75.0,
            "chemical_usage_score": 40.0,
            "energy_consumption_score": 60.0,
            "water_liters_per_kg": 3500.0,
            "co2_kg_per_kg": 2.5,
            "energy_mj_per_kg": 55.0,
            "is_biodegradable": "Partially",
            "recycling_potential": "Low",
            "durability_rating": 6.0,
            "typical_wash_temp": "cold",
            "drying_method": "air dry"
        },
        {
            "name": "Nylon",
            "material_type": MaterialType.SYNTHETIC,
            "description": "Petroleum-based synthetic with high environmental impact",
            "water_usage_score": 80.0,
            "carbon_footprint_score": 25.0,
            "biodegradability_score": 5.0,
            "chemical_usage_score": 30.0,
            "energy_consumption_score": 20.0,
            "water_liters_per_kg": 100.0,
            "co2_kg_per_kg": 8.5,
            "energy_mj_per_kg": 150.0,
            "is_biodegradable": "No",
            "recycling_potential": "Low",
            "durability_rating": 9.0,
            "typical_wash_temp": "cold",
            "drying_method": "low heat"
        }
    ]

    count = 0
    for data in materials_data:
        existing = db.query(Material).filter(Material.name == data["name"]).first()
        if not existing:
            material = Material(**data)
            db.add(material)
            count += 1

    db.commit()
    logger.info(f"Loaded {count} materials")
    return count


def load_garments(db: Session) -> int:
    """Load garment fixtures."""
    # Get materials for associations
    organic_cotton = db.query(Material).filter(Material.name == "Organic Cotton").first()
    conventional_cotton = db.query(Material).filter(Material.name == "Conventional Cotton").first()
    polyester = db.query(Material).filter(Material.name == "Polyester").first()
    recycled_poly = db.query(Material).filter(Material.name == "Recycled Polyester").first()
    linen = db.query(Material).filter(Material.name == "Linen").first()
    hemp = db.query(Material).filter(Material.name == "Hemp").first()
    tencel = db.query(Material).filter(Material.name == "Tencel (Lyocell)").first()

    garments_data = [
        {
            "name": "Organic Cotton T-Shirt",
            "category": "shirt",
            "description": "Basic t-shirt made from 100% organic cotton",
            "typical_weight_kg": 0.15,
            "expected_lifespan_years": 3.0,
            "typical_wears": 100,
            "care_instructions": "Machine wash cold, tumble dry low or air dry",
            "washing_frequency": "After 1-2 wears",
            "materials": [organic_cotton] if organic_cotton else []
        },
        {
            "name": "Conventional Cotton T-Shirt",
            "category": "shirt",
            "description": "Standard t-shirt made from conventional cotton",
            "typical_weight_kg": 0.15,
            "expected_lifespan_years": 2.0,
            "typical_wears": 50,
            "care_instructions": "Machine wash warm, tumble dry",
            "washing_frequency": "After each wear",
            "materials": [conventional_cotton] if conventional_cotton else []
        },
        {
            "name": "Linen Summer Shirt",
            "category": "shirt",
            "description": "Breathable linen shirt perfect for warm weather",
            "typical_weight_kg": 0.2,
            "expected_lifespan_years": 5.0,
            "typical_wears": 150,
            "care_instructions": "Hand wash or machine wash cold, air dry",
            "washing_frequency": "After 2-3 wears",
            "materials": [linen] if linen else []
        },
        {
            "name": "Hemp Jeans",
            "category": "pants",
            "description": "Durable jeans made from hemp blend",
            "typical_weight_kg": 0.6,
            "expected_lifespan_years": 7.0,
            "typical_wears": 300,
            "care_instructions": "Machine wash cold inside out, air dry",
            "washing_frequency": "After 5-10 wears",
            "materials": [hemp] if hemp else []
        },
        {
            "name": "Recycled Polyester Jacket",
            "category": "outerwear",
            "description": "Lightweight jacket made from recycled plastic bottles",
            "typical_weight_kg": 0.4,
            "expected_lifespan_years": 4.0,
            "typical_wears": 200,
            "care_instructions": "Machine wash cold, air dry preferred",
            "washing_frequency": "After 5-10 wears or as needed",
            "materials": [recycled_poly] if recycled_poly else []
        },
        {
            "name": "Polyester Athletic Wear",
            "category": "activewear",
            "description": "Sports clothing made from conventional polyester",
            "typical_weight_kg": 0.2,
            "expected_lifespan_years": 1.5,
            "typical_wears": 75,
            "care_instructions": "Machine wash cold, low heat dry",
            "washing_frequency": "After each wear",
            "materials": [polyester] if polyester else []
        },
        {
            "name": "Tencel Summer Dress",
            "category": "dress",
            "description": "Flowing dress made from sustainable Tencel fabric",
            "typical_weight_kg": 0.3,
            "expected_lifespan_years": 4.0,
            "typical_wears": 120,
            "care_instructions": "Hand wash or delicate cycle cold, air dry",
            "washing_frequency": "After 1-2 wears",
            "materials": [tencel] if tencel else []
        }
    ]

    count = 0
    for data in garments_data:
        existing = db.query(Garment).filter(Garment.name == data["name"]).first()
        if not existing:
            materials = data.pop("materials", [])
            garment = Garment(**data)
            garment.materials.extend(materials)
            garment.sustainability_score = garment.calculate_sustainability_score()
            db.add(garment)
            count += 1

    db.commit()
    logger.info(f"Loaded {count} garments")
    return count


def load_brands(db: Session) -> int:
    """Load brand fixtures."""
    brands_data = [
        {
            "name": "Patagonia",
            "description": "Outdoor clothing company committed to environmental activism and sustainable practices",
            "website": "https://www.patagonia.com",
            "overall_rating": 88.0,
            "environmental_rating": 90.0,
            "labor_rating": 85.0,
            "animal_welfare_rating": 90.0,
            "is_certified_bcorp": True,
            "is_fair_trade": True,
            "is_organic_certified": True,
            "uses_recycled_materials": True,
            "carbon_neutral": False,
            "country": "USA",
            "price_range": "$$$",
            "transparency_score": 95.0,
            "good_on_you_rating": "Great"
        },
        {
            "name": "Eileen Fisher",
            "description": "Women's clothing brand focused on sustainability and circular fashion",
            "website": "https://www.eileenfisher.com",
            "overall_rating": 85.0,
            "environmental_rating": 88.0,
            "labor_rating": 82.0,
            "animal_welfare_rating": 85.0,
            "is_certified_bcorp": True,
            "is_fair_trade": True,
            "is_organic_certified": True,
            "uses_recycled_materials": True,
            "carbon_neutral": False,
            "country": "USA",
            "price_range": "$$$$",
            "transparency_score": 90.0,
            "good_on_you_rating": "Great"
        },
        {
            "name": "Reformation",
            "description": "Fashion brand focused on sustainable materials and transparent practices",
            "website": "https://www.thereformation.com",
            "overall_rating": 80.0,
            "environmental_rating": 85.0,
            "labor_rating": 75.0,
            "animal_welfare_rating": 80.0,
            "is_certified_bcorp": False,
            "is_fair_trade": False,
            "is_organic_certified": False,
            "uses_recycled_materials": True,
            "carbon_neutral": True,
            "country": "USA",
            "price_range": "$$$",
            "transparency_score": 85.0,
            "good_on_you_rating": "Good"
        },
        {
            "name": "People Tree",
            "description": "Pioneer in sustainable and Fair Trade fashion",
            "website": "https://www.peopletree.co.uk",
            "overall_rating": 87.0,
            "environmental_rating": 85.0,
            "labor_rating": 90.0,
            "animal_welfare_rating": 85.0,
            "is_certified_bcorp": False,
            "is_fair_trade": True,
            "is_organic_certified": True,
            "uses_recycled_materials": False,
            "carbon_neutral": False,
            "country": "UK",
            "price_range": "$$",
            "transparency_score": 92.0,
            "good_on_you_rating": "Great"
        },
        {
            "name": "Zara",
            "description": "Fast fashion brand with some sustainability initiatives",
            "website": "https://www.zara.com",
            "overall_rating": 35.0,
            "environmental_rating": 30.0,
            "labor_rating": 35.0,
            "animal_welfare_rating": 40.0,
            "is_certified_bcorp": False,
            "is_fair_trade": False,
            "is_organic_certified": False,
            "uses_recycled_materials": True,
            "carbon_neutral": False,
            "country": "Spain",
            "price_range": "$$",
            "transparency_score": 40.0,
            "good_on_you_rating": "Not good enough"
        },
        {
            "name": "H&M",
            "description": "Fast fashion retailer with Conscious Collection",
            "website": "https://www.hm.com",
            "overall_rating": 38.0,
            "environmental_rating": 35.0,
            "labor_rating": 40.0,
            "animal_welfare_rating": 40.0,
            "is_certified_bcorp": False,
            "is_fair_trade": False,
            "is_organic_certified": False,
            "uses_recycled_materials": True,
            "carbon_neutral": False,
            "country": "Sweden",
            "price_range": "$",
            "transparency_score": 45.0,
            "good_on_you_rating": "It's a start"
        }
    ]

    count = 0
    for data in brands_data:
        existing = db.query(Brand).filter(Brand.name == data["name"]).first()
        if not existing:
            brand = Brand(**data)
            db.add(brand)
            count += 1

    db.commit()
    logger.info(f"Loaded {count} brands")
    return count


def load_all_fixtures(db: Session) -> dict:
    """
    Load all fixtures into the database.

    Returns:
        Dictionary with counts of loaded items
    """
    logger.info("Loading all fixtures...")

    counts = {
        "materials": load_materials(db),
        "garments": load_garments(db),
        "brands": load_brands(db)
    }

    logger.info(f"Fixtures loaded successfully: {counts}")
    return counts


if __name__ == "__main__":
    # Initialize database
    init_db()

    # Load fixtures
    from models import SessionLocal
    db = SessionLocal()
    try:
        counts = load_all_fixtures(db)
        print(f"\n✅ Fixtures loaded successfully!")
        print(f"   Materials: {counts['materials']}")
        print(f"   Garments: {counts['garments']}")
        print(f"   Brands: {counts['brands']}")
    finally:
        db.close()
