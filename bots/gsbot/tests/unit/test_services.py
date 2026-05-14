"""Unit tests for services."""

import pytest
from sqlalchemy import create_engine
from sqlalchemy.orm import sessionmaker

from models import Base, Material, MaterialType
from services.database import MaterialService
from services.sustainability import SustainabilityAnalyzer


@pytest.fixture
def db_session():
    """Create a test database session."""
    engine = create_engine("sqlite:///:memory:")
    Base.metadata.create_all(engine)
    SessionLocal = sessionmaker(bind=engine)
    session = SessionLocal()
    yield session
    session.close()


class TestMaterialService:
    """Tests for MaterialService."""

    def test_create_material(self, db_session):
        """Test creating a material through service."""
        material = Material(
            name="Test Material",
            material_type=MaterialType.NATURAL
        )
        created = MaterialService.create(db_session, material)

        assert created.id is not None
        assert created.name == "Test Material"

    def test_get_by_name(self, db_session):
        """Test getting material by name."""
        material = Material(
            name="Organic Cotton",
            material_type=MaterialType.ORGANIC
        )
        db_session.add(material)
        db_session.commit()

        found = MaterialService.get_by_name(db_session, "organic")
        assert found is not None
        assert "Cotton" in found.name

    def test_search(self, db_session):
        """Test material search."""
        materials = [
            Material(name="Cotton", material_type=MaterialType.NATURAL, description="Natural fiber"),
            Material(name="Polyester", material_type=MaterialType.SYNTHETIC, description="Synthetic fiber"),
            Material(name="Organic Cotton", material_type=MaterialType.ORGANIC, description="Organic natural fiber"),
        ]

        for mat in materials:
            db_session.add(mat)
        db_session.commit()

        results = MaterialService.search(db_session, "cotton")
        assert len(results) == 2  # Should find both cotton materials


class TestSustainabilityAnalyzer:
    """Tests for SustainabilityAnalyzer."""

    def test_get_impact_category(self):
        """Test impact category assignment."""
        analyzer = SustainabilityAnalyzer()

        assert "Very Low" in analyzer.get_impact_category(90.0)
        assert "Low" in analyzer.get_impact_category(70.0)
        assert "Medium" in analyzer.get_impact_category(50.0)
        assert "High" in analyzer.get_impact_category(30.0)

    def test_get_material_recommendation(self):
        """Test material recommendation."""
        analyzer = SustainabilityAnalyzer()

        good_material = Material(
            name="Excellent",
            material_type=MaterialType.ORGANIC,
            water_usage_score=90.0,
            carbon_footprint_score=90.0,
            biodegradability_score=90.0,
            chemical_usage_score=90.0,
            energy_consumption_score=90.0
        )

        recommendation = analyzer.get_material_recommendation(good_material)
        assert "Excellent" in recommendation

        bad_material = Material(
            name="Poor",
            material_type=MaterialType.SYNTHETIC,
            water_usage_score=30.0,
            carbon_footprint_score=30.0,
            biodegradability_score=30.0,
            chemical_usage_score=30.0,
            energy_consumption_score=30.0
        )

        recommendation = analyzer.get_material_recommendation(bad_material)
        assert "Not recommended" in recommendation

    def test_get_sustainability_tips(self):
        """Test getting sustainability tips."""
        analyzer = SustainabilityAnalyzer()
        tips = analyzer.get_sustainability_tips()

        assert len(tips) > 0
        assert isinstance(tips, list)
        assert all(isinstance(tip, str) for tip in tips)

    def test_compare_materials(self):
        """Test material comparison."""
        analyzer = SustainabilityAnalyzer()

        mat1 = Material(
            name="Material 1",
            material_type=MaterialType.ORGANIC,
            water_usage_score=80.0,
            carbon_footprint_score=70.0,
            biodegradability_score=90.0,
            chemical_usage_score=75.0,
            energy_consumption_score=85.0
        )

        mat2 = Material(
            name="Material 2",
            material_type=MaterialType.SYNTHETIC,
            water_usage_score=60.0,
            carbon_footprint_score=50.0,
            biodegradability_score=40.0,
            chemical_usage_score=45.0,
            energy_consumption_score=55.0
        )

        comparison = analyzer.compare_materials(mat1, mat2)

        assert comparison["material1"] == "Material 1"
        assert comparison["material2"] == "Material 2"
        assert comparison["water_usage"]["better"] == "Material 1"
        assert "overall_scores" in comparison
