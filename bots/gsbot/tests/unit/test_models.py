"""Unit tests for data models."""

import pytest
from sqlalchemy import create_engine
from sqlalchemy.orm import sessionmaker

from models import Base, Material, MaterialType, Garment, Brand, User


@pytest.fixture
def db_session():
    """Create a test database session."""
    engine = create_engine("sqlite:///:memory:")
    Base.metadata.create_all(engine)
    SessionLocal = sessionmaker(bind=engine)
    session = SessionLocal()
    yield session
    session.close()


class TestMaterial:
    """Tests for Material model."""

    def test_create_material(self, db_session):
        """Test creating a material."""
        material = Material(
            name="Test Cotton",
            material_type=MaterialType.NATURAL,
            water_usage_score=70.0,
            carbon_footprint_score=65.0,
            biodegradability_score=90.0,
            chemical_usage_score=75.0,
            energy_consumption_score=70.0
        )
        db_session.add(material)
        db_session.commit()

        assert material.id is not None
        assert material.name == "Test Cotton"

    def test_calculate_overall_score(self):
        """Test sustainability score calculation."""
        material = Material(
            name="Test",
            material_type=MaterialType.NATURAL,
            water_usage_score=80.0,
            carbon_footprint_score=70.0,
            biodegradability_score=90.0,
            chemical_usage_score=60.0,
            energy_consumption_score=75.0
        )

        score = material.calculate_overall_score()
        assert score == 75.0  # Average of all scores

    def test_get_grade(self):
        """Test grade assignment."""
        material = Material(
            name="Test",
            material_type=MaterialType.NATURAL,
            water_usage_score=90.0,
            carbon_footprint_score=90.0,
            biodegradability_score=90.0,
            chemical_usage_score=90.0,
            energy_consumption_score=90.0
        )

        assert material.get_grade() == "A+"

        material.water_usage_score = 50.0
        material.carbon_footprint_score = 50.0
        material.biodegradability_score = 50.0
        material.chemical_usage_score = 50.0
        material.energy_consumption_score = 50.0

        assert material.get_grade() == "D"


class TestGarment:
    """Tests for Garment model."""

    def test_create_garment(self, db_session):
        """Test creating a garment."""
        garment = Garment(
            name="Test T-Shirt",
            category="shirt",
            typical_weight_kg=0.15,
            expected_lifespan_years=3.0
        )
        db_session.add(garment)
        db_session.commit()

        assert garment.id is not None
        assert garment.name == "Test T-Shirt"

    def test_calculate_sustainability_score(self, db_session):
        """Test garment sustainability calculation."""
        # Create materials
        mat1 = Material(
            name="Good Material",
            material_type=MaterialType.ORGANIC,
            water_usage_score=80.0,
            carbon_footprint_score=80.0,
            biodegradability_score=80.0,
            chemical_usage_score=80.0,
            energy_consumption_score=80.0
        )
        db_session.add(mat1)
        db_session.commit()

        # Create garment with material
        garment = Garment(
            name="Test Garment",
            category="shirt",
            expected_lifespan_years=5.0
        )
        garment.materials.append(mat1)

        score = garment.calculate_sustainability_score()
        assert score > 80.0  # Should be boosted by long lifespan


class TestBrand:
    """Tests for Brand model."""

    def test_create_brand(self, db_session):
        """Test creating a brand."""
        brand = Brand(
            name="Test Brand",
            overall_rating=85.0,
            environmental_rating=80.0,
            labor_rating=85.0,
            animal_welfare_rating=90.0
        )
        db_session.add(brand)
        db_session.commit()

        assert brand.id is not None
        assert brand.name == "Test Brand"

    def test_get_certification_badges(self):
        """Test certification badges."""
        brand = Brand(
            name="Test",
            is_certified_bcorp=True,
            is_fair_trade=True,
            uses_recycled_materials=False
        )

        badges = brand.get_certification_badges()
        assert "B Corp Certified" in badges
        assert "Fair Trade" in badges
        assert len(badges) == 2

    def test_get_rating_summary(self):
        """Test rating summary."""
        brand = Brand(name="Test", overall_rating=85.0)
        summary = brand.get_rating_summary()
        assert "Excellent" in summary

        brand.overall_rating = 45.0
        summary = brand.get_rating_summary()
        assert "progress" in summary.lower()


class TestUser:
    """Tests for User model."""

    def test_create_user(self, db_session):
        """Test creating a user."""
        user = User(
            discord_id=123456789,
            username="testuser"
        )
        db_session.add(user)
        db_session.commit()

        assert user.id is not None
        assert user.discord_id == 123456789

    def test_add_points(self):
        """Test adding points and leveling up."""
        user = User(
            discord_id=123,
            username="test",
            sustainability_points=0,
            level=1
        )

        user.add_points(50)
        assert user.sustainability_points == 50
        assert user.level == 1

        user.add_points(60)
        assert user.sustainability_points == 110
        assert user.level == 2  # Should level up at 100 points

    def test_get_rank(self):
        """Test rank assignment."""
        user = User(discord_id=123, username="test", level=1)
        assert user.get_rank() == "Sustainability Learner"

        user.level = 10
        assert user.get_rank() == "Green Enthusiast"

        user.level = 20
        assert user.get_rank() == "Sustainability Champion"
