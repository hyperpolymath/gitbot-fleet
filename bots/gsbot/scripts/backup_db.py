"""Script to backup the database."""

import sys
import shutil
from pathlib import Path
from datetime import datetime

# Add src to path
sys.path.insert(0, str(Path(__file__).parent.parent / "src"))

from config.settings import config
from utils.logger import setup_logger

logger = setup_logger("backup", "INFO")


def backup_database():
    """Create a backup of the database."""
    try:
        # Get database path
        db_path = config.get_database_path()

        if not db_path.exists():
            logger.error(f"Database file not found: {db_path}")
            print(f"❌ Database file not found: {db_path}")
            return False

        # Create backup directory
        backup_dir = config.DATA_DIR / "backups"
        backup_dir.mkdir(parents=True, exist_ok=True)

        # Generate backup filename with timestamp
        timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
        backup_filename = f"gsbot_backup_{timestamp}.db"
        backup_path = backup_dir / backup_filename

        # Copy database file
        shutil.copy2(db_path, backup_path)

        # Get file sizes
        original_size = db_path.stat().st_size
        backup_size = backup_path.stat().st_size

        logger.info(f"Database backed up successfully to {backup_path}")
        print(f"\n✅ Database backup completed!")
        print(f"   Source: {db_path}")
        print(f"   Backup: {backup_path}")
        print(f"   Size: {backup_size:,} bytes")

        # Clean up old backups (keep last 10)
        cleanup_old_backups(backup_dir, keep=10)

        return True

    except Exception as e:
        logger.error(f"Backup failed: {e}", exc_info=True)
        print(f"❌ Backup failed: {e}")
        return False


def cleanup_old_backups(backup_dir: Path, keep: int = 10):
    """
    Remove old backup files, keeping only the most recent ones.

    Args:
        backup_dir: Directory containing backups
        keep: Number of recent backups to keep
    """
    try:
        backups = sorted(
            backup_dir.glob("gsbot_backup_*.db"),
            key=lambda p: p.stat().st_mtime,
            reverse=True
        )

        if len(backups) > keep:
            removed_count = 0
            for old_backup in backups[keep:]:
                old_backup.unlink()
                removed_count += 1
                logger.info(f"Removed old backup: {old_backup.name}")

            if removed_count > 0:
                print(f"\n🗑️  Removed {removed_count} old backup(s)")

    except Exception as e:
        logger.error(f"Cleanup failed: {e}")


def restore_database(backup_path: str):
    """
    Restore database from a backup.

    Args:
        backup_path: Path to backup file
    """
    try:
        backup_file = Path(backup_path)

        if not backup_file.exists():
            logger.error(f"Backup file not found: {backup_path}")
            print(f"❌ Backup file not found: {backup_path}")
            return False

        # Get current database path
        db_path = config.get_database_path()

        # Create backup of current database before restoring
        if db_path.exists():
            temp_backup = db_path.parent / f"{db_path.name}.before_restore"
            shutil.copy2(db_path, temp_backup)
            print(f"📁 Current database backed up to: {temp_backup}")

        # Restore from backup
        shutil.copy2(backup_file, db_path)

        logger.info(f"Database restored from {backup_file}")
        print(f"\n✅ Database restored successfully!")
        print(f"   From: {backup_file}")
        print(f"   To: {db_path}")

        return True

    except Exception as e:
        logger.error(f"Restore failed: {e}", exc_info=True)
        print(f"❌ Restore failed: {e}")
        return False


if __name__ == "__main__":
    if len(sys.argv) > 1 and sys.argv[1] == "restore":
        if len(sys.argv) < 3:
            print("Usage: python backup_db.py restore <backup_file>")
            sys.exit(1)
        restore_database(sys.argv[2])
    else:
        backup_database()
