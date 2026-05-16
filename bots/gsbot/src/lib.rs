// SPDX-License-Identifier: PMPL-1.0-or-later
//! Garment Sustainability Bot — Rust/SPARK port of the original Python
//! Discord bot. Library crate exposing the domain kernel, persistence,
//! services, and the poise/serenity command surface.

pub mod cache;
pub mod commands;
pub mod config;
pub mod db;
pub mod domain;
pub mod fixtures;
pub mod logging;
pub mod models;
pub mod services;
pub mod sustainability;

use sqlx::SqlitePool;

use crate::config::Config;

/// Per-process error type for poise commands.
pub type Error = anyhow::Error;
/// poise command context, parameterised over our shared [`Data`].
pub type Context<'a> = poise::Context<'a, Data, Error>;

/// Shared state handed to every command (the serenity/poise analogue of
/// the discord.py bot holding config + a DB handle).
pub struct Data {
    pub db: SqlitePool,
    pub config: Config,
}

/// Best-effort "typing…" indicator, mirroring `async with ctx.typing()`.
pub async fn typing(ctx: &Context<'_>) {
    let _ = ctx.channel_id().broadcast_typing(ctx.http()).await;
}
