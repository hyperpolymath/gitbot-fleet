// SPDX-License-Identifier: PMPL-1.0-or-later
//! Admin cog: `loaddata`, `stats`, `announce`. Gated by [`super::is_admin`].

use poise::serenity_prelude as serenity;

use super::{colour, say, send_embed};
use crate::services::{BrandService, GarmentService, MaterialService, UserService};
use crate::{fixtures, typing, Context, Error};

/// Load sample data into the database.
#[poise::command(prefix_command, check = "super::is_admin")]
pub async fn loaddata(ctx: Context<'_>) -> Result<(), Error> {
    typing(&ctx).await;
    say(&ctx, "⏳ Loading data fixtures...").await?;

    let counts = fixtures::load_all_fixtures(&ctx.data().db).await?;
    let embed = serenity::CreateEmbed::new()
        .title("✅ Data Loaded Successfully")
        .colour(colour::green())
        .field("Materials", counts.materials.to_string(), true)
        .field("Garments", counts.garments.to_string(), true)
        .field("Brands", counts.brands.to_string(), true);
    send_embed(&ctx, embed).await
}

/// Show bot statistics.
#[poise::command(prefix_command, check = "super::is_admin")]
pub async fn stats(ctx: Context<'_>) -> Result<(), Error> {
    typing(&ctx).await;
    let db = &ctx.data().db;

    let materials = MaterialService::count(db).await?;
    let garments = GarmentService::count(db).await?;
    let brands = BrandService::count(db).await?;
    let users = UserService::count(db).await?;

    let guilds = ctx.cache().guild_count();
    // Faithful equivalent of discord.py's `bot.latency * 1000`: poise's
    // `ping()` is the gateway heartbeat latency (zero until the shard has
    // had its first heartbeat ack).
    let latency_ms = ctx.ping().await.as_secs_f64() * 1000.0;

    let embed = serenity::CreateEmbed::new()
        .title("📊 Bot Statistics")
        .colour(colour::blue())
        .field("Guilds", guilds.to_string(), true)
        .field("Users Tracked", users.to_string(), true)
        .field("Latency", format!("{latency_ms:.0}ms"), true)
        .field("Materials", materials.to_string(), true)
        .field("Garments", garments.to_string(), true)
        .field("Brands", brands.to_string(), true);
    send_embed(&ctx, embed).await
}

/// Send an announcement to all guilds.
#[poise::command(prefix_command, check = "super::is_admin")]
pub async fn announce(
    ctx: Context<'_>,
    #[rest]
    #[description = "Announcement message"]
    message: String,
) -> Result<(), Error> {
    let mut sent = 0_u32;
    let mut failed = 0_u32;

    let guild_ids = ctx.cache().guilds();
    for gid in guild_ids {
        // Resolve a target channel: prefer one named "general", else the
        // first text channel (mirrors the Python logic).
        let target = {
            let guild = match gid.to_guild_cached(ctx.cache()) {
                Some(g) => g,
                None => {
                    failed += 1;
                    continue;
                }
            };
            let mut text: Vec<_> = guild
                .channels
                .iter()
                .filter(|(_, c)| c.kind == serenity::ChannelType::Text)
                .map(|(id, c)| (*id, c.name.clone()))
                .collect();
            text.sort_by_key(|(id, _)| *id);
            text.iter()
                .find(|(_, n)| n == "general")
                .or_else(|| text.first())
                .map(|(id, _)| *id)
        };

        match target {
            Some(channel_id) => {
                let embed = serenity::CreateEmbed::new()
                    .title("📢 Announcement")
                    .description(&message)
                    .colour(colour::blue())
                    .footer(serenity::CreateEmbedFooter::new(
                        "Garment Sustainability Bot",
                    ));
                let msg = serenity::CreateMessage::new().embed(embed);
                if channel_id.send_message(ctx.http(), msg).await.is_ok() {
                    sent += 1;
                } else {
                    failed += 1;
                }
            }
            None => failed += 1,
        }
    }

    say(
        &ctx,
        format!("✅ Announcement sent to {sent} guild(s). Failed: {failed}"),
    )
    .await
}
