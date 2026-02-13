"""Integration tests for bot commands."""

import pytest
from unittest.mock import AsyncMock, MagicMock, patch
from sqlalchemy import create_engine
from sqlalchemy.orm import sessionmaker

from models import Base, Material, MaterialType, Garment
from bot.cogs.sustainability import SustainabilityCommands
from bot.cogs.materials import MaterialsCommands


@pytest.fixture
def db_session():
    """Create test database session."""
    engine = create_engine("sqlite:///:memory:")
    Base.metadata.create_all(engine)
    SessionLocal = sessionmaker(bind=engine)
    session = SessionLocal()

    # Add test data
    material = Material(
        name="Test Cotton",
        material_type=MaterialType.ORGANIC,
        description="Test material",
        water_usage_score=75.0,
        carbon_footprint_score=80.0,
        biodegradability_score=90.0,
        chemical_usage_score=85.0,
        energy_consumption_score=70.0
    )
    session.add(material)

    garment = Garment(
        name="Test T-Shirt",
        category="shirt",
        description="Test garment",
        typical_weight_kg=0.15,
        expected_lifespan_years=3.0
    )
    garment.materials.append(material)
    garment.sustainability_score = garment.calculate_sustainability_score()
    session.add(garment)

    session.commit()

    yield session
    session.close()


@pytest.mark.asyncio
async def test_sustainability_command(db_session):
    """Test sustainability command."""
    bot = MagicMock()
    cog = SustainabilityCommands(bot)

    ctx = AsyncMock()
    ctx.author.id = 12345
    ctx.author.name = "TestUser"

    with patch('bot.cogs.sustainability.get_db', return_value=iter([db_session])):
        await cog.sustainability_score(ctx, garment_name="Test T-Shirt")

    # Verify send was called
    assert ctx.send.called
    call_args = ctx.send.call_args

    # Check that an embed was sent
    if call_args and call_args[1]:
        embed = call_args[1].get('embed')
        if embed:
            assert "Test T-Shirt" in embed.title or "T-Shirt" in str(embed.to_dict())


@pytest.mark.asyncio
async def test_impact_command(db_session):
    """Test material impact command."""
    bot = MagicMock()
    cog = MaterialsCommands(bot)

    ctx = AsyncMock()
    ctx.author.id = 12345
    ctx.author.name = "TestUser"

    with patch('bot.cogs.materials.get_db', return_value=iter([db_session])):
        await cog.material_impact(ctx, material_name="Test Cotton")

    # Verify send was called
    assert ctx.send.called


@pytest.mark.asyncio
async def test_search_command(db_session):
    """Test search command."""
    bot = MagicMock()
    cog = MaterialsCommands(bot)

    ctx = AsyncMock()

    with patch('bot.cogs.materials.get_db', return_value=iter([db_session])):
        await cog.search(ctx, query="Test")

    # Verify send was called with results
    assert ctx.send.called
