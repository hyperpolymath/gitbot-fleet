"""Database service for CRUD operations."""

from typing import Optional, List
from sqlalchemy.orm import Session
from sqlalchemy import or_

from models import Material, Garment, Brand, User, garment_materials
from utils.logger import get_logger

logger = get_logger(__name__)


class MaterialService:
    """Service for material-related database operations."""

    @staticmethod
    def get_by_name(db: Session, name: str) -> Optional[Material]:
        """Get material by name."""
        return db.query(Material).filter(Material.name.ilike(f"%{name}%")).first()

    @staticmethod
    def get_all(db: Session, skip: int = 0, limit: int = 100) -> List[Material]:
        """Get all materials."""
        return db.query(Material).offset(skip).limit(limit).all()

    @staticmethod
    def create(db: Session, material: Material) -> Material:
        """Create a new material."""
        db.add(material)
        db.commit()
        db.refresh(material)
        logger.info(f"Created material: {material.name}")
        return material

    @staticmethod
    def search(db: Session, query: str) -> List[Material]:
        """Search materials by name or description."""
        return db.query(Material).filter(
            or_(
                Material.name.ilike(f"%{query}%"),
                Material.description.ilike(f"%{query}%")
            )
        ).all()


class GarmentService:
    """Service for garment-related database operations."""

    @staticmethod
    def get_by_name(db: Session, name: str) -> Optional[Garment]:
        """Get garment by name."""
        return db.query(Garment).filter(Garment.name.ilike(f"%{name}%")).first()

    @staticmethod
    def get_by_category(db: Session, category: str) -> List[Garment]:
        """Get garments by category."""
        return db.query(Garment).filter(Garment.category.ilike(f"%{category}%")).all()

    @staticmethod
    def get_all(db: Session, skip: int = 0, limit: int = 100) -> List[Garment]:
        """Get all garments."""
        return db.query(Garment).offset(skip).limit(limit).all()

    @staticmethod
    def create(db: Session, garment: Garment) -> Garment:
        """Create a new garment."""
        db.add(garment)
        db.commit()
        db.refresh(garment)
        logger.info(f"Created garment: {garment.name}")
        return garment

    @staticmethod
    def search(db: Session, query: str) -> List[Garment]:
        """Search garments by name, category, or description."""
        return db.query(Garment).filter(
            or_(
                Garment.name.ilike(f"%{query}%"),
                Garment.category.ilike(f"%{query}%"),
                Garment.description.ilike(f"%{query}%")
            )
        ).all()

    @staticmethod
    def get_alternatives(db: Session, garment: Garment) -> List[Garment]:
        """Get sustainable alternatives for a garment."""
        # Get garments in same category with better sustainability score
        current_score = garment.calculate_sustainability_score()
        return db.query(Garment).filter(
            Garment.category == garment.category,
            Garment.id != garment.id,
            Garment.sustainability_score > current_score
        ).order_by(Garment.sustainability_score.desc()).limit(5).all()


class BrandService:
    """Service for brand-related database operations."""

    @staticmethod
    def get_by_name(db: Session, name: str) -> Optional[Brand]:
        """Get brand by name."""
        return db.query(Brand).filter(Brand.name.ilike(f"%{name}%")).first()

    @staticmethod
    def get_all(db: Session, skip: int = 0, limit: int = 100) -> List[Brand]:
        """Get all brands."""
        return db.query(Brand).offset(skip).limit(limit).all()

    @staticmethod
    def create(db: Session, brand: Brand) -> Brand:
        """Create a new brand."""
        db.add(brand)
        db.commit()
        db.refresh(brand)
        logger.info(f"Created brand: {brand.name}")
        return brand

    @staticmethod
    def search(db: Session, query: str) -> List[Brand]:
        """Search brands by name or description."""
        return db.query(Brand).filter(
            or_(
                Brand.name.ilike(f"%{query}%"),
                Brand.description.ilike(f"%{query}%")
            )
        ).all()

    @staticmethod
    def get_top_rated(db: Session, limit: int = 10) -> List[Brand]:
        """Get top-rated sustainable brands."""
        return db.query(Brand).order_by(Brand.overall_rating.desc()).limit(limit).all()


class UserService:
    """Service for user-related database operations."""

    @staticmethod
    def get_by_discord_id(db: Session, discord_id: int) -> Optional[User]:
        """Get user by Discord ID."""
        return db.query(User).filter(User.discord_id == discord_id).first()

    @staticmethod
    def create(db: Session, discord_id: int, username: str) -> User:
        """Create a new user."""
        user = User(discord_id=discord_id, username=username)
        db.add(user)
        db.commit()
        db.refresh(user)
        logger.info(f"Created user: {username} ({discord_id})")
        return user

    @staticmethod
    def get_or_create(db: Session, discord_id: int, username: str) -> User:
        """Get existing user or create new one."""
        user = UserService.get_by_discord_id(db, discord_id)
        if not user:
            user = UserService.create(db, discord_id, username)
        return user

    @staticmethod
    def update(db: Session, user: User) -> User:
        """Update user."""
        db.commit()
        db.refresh(user)
        return user

    @staticmethod
    def get_leaderboard(db: Session, limit: int = 10) -> List[User]:
        """Get top users by sustainability points."""
        return db.query(User).order_by(User.sustainability_points.desc()).limit(limit).all()
