// SPDX-License-Identifier: MPL-2.0
//! `gsbot-backup-db` — port of `scripts/backup_db.py`. With no args it
//! backs up the SQLite database (keeping the last 10); `restore <file>`
//! restores from a backup.

use anyhow::{bail, Result};

use gsbot::config::Config;

fn cleanup_old_backups(dir: &std::path::Path, keep: usize) -> Result<()> {
    let mut backups: Vec<_> = std::fs::read_dir(dir)?
        .filter_map(|e| e.ok())
        .filter(|e| {
            e.file_name()
                .to_string_lossy()
                .starts_with("gsbot_backup_")
        })
        .collect();
    backups.sort_by_key(|e| {
        e.metadata()
            .and_then(|m| m.modified())
            .ok()
    });
    backups.reverse();
    if backups.len() > keep {
        let mut removed = 0;
        for old in &backups[keep..] {
            if std::fs::remove_file(old.path()).is_ok() {
                removed += 1;
            }
        }
        if removed > 0 {
            println!("\n🗑️  Removed {removed} old backup(s)");
        }
    }
    Ok(())
}

fn backup(config: &Config) -> Result<()> {
    let db_path = config.database_path()?;
    if !db_path.exists() {
        bail!("Database file not found: {}", db_path.display());
    }
    let backup_dir = config.data_dir().join("backups");
    std::fs::create_dir_all(&backup_dir)?;
    let ts = chrono::Local::now().format("%Y%m%d_%H%M%S").to_string();
    let backup_path = backup_dir.join(format!("gsbot_backup_{ts}.db"));
    std::fs::copy(&db_path, &backup_path)?;
    let size = std::fs::metadata(&backup_path)?.len();

    println!("\n✅ Database backup completed!");
    println!("   Source: {}", db_path.display());
    println!("   Backup: {}", backup_path.display());
    println!("   Size: {size} bytes");

    cleanup_old_backups(&backup_dir, 10)?;
    Ok(())
}

fn restore(config: &Config, backup_path: &str) -> Result<()> {
    let backup_file = std::path::Path::new(backup_path);
    if !backup_file.exists() {
        bail!("Backup file not found: {backup_path}");
    }
    let db_path = config.database_path()?;
    if db_path.exists() {
        let tmp = db_path.with_file_name(format!(
            "{}.before_restore",
            db_path.file_name().unwrap_or_default().to_string_lossy()
        ));
        std::fs::copy(&db_path, &tmp)?;
        println!("📁 Current database backed up to: {}", tmp.display());
    }
    std::fs::copy(backup_file, &db_path)?;
    println!("\n✅ Database restored successfully!");
    println!("   From: {}", backup_file.display());
    println!("   To: {}", db_path.display());
    Ok(())
}

fn main() -> Result<()> {
    let config = Config::from_env();
    let args: Vec<String> = std::env::args().collect();
    if args.get(1).map(String::as_str) == Some("restore") {
        match args.get(2) {
            Some(path) => restore(&config, path),
            None => {
                eprintln!("Usage: gsbot-backup-db restore <backup_file>");
                std::process::exit(1);
            }
        }
    } else {
        backup(&config)
    }
}
