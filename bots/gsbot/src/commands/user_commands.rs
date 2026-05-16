// SPDX-License-Identifier: PMPL-1.0-or-later
//! User cog: `profile`, `leaderboard`, `setpreference`.

use poise::serenity_prelude as serenity;

use super::{colour, say, send_embed};
use crate::services::UserService;
use crate::{typing, Context, Error};

fn title_case(s: &str) -> String {
    s.split('_')
        .map(|w| {
            let mut c = w.chars();
            match c.next() {
                Some(f) => f.to_uppercase().chain(c).collect::<String>(),
                None => String::new(),
            }
        })
        .collect::<Vec<_>>()
        .join(" ")
}

/// View your sustainability profile.
#[poise::command(prefix_command, aliases("stats", "me"))]
pub async fn profile(ctx: Context<'_>) -> Result<(), Error> {
    typing(&ctx).await;
    let db = &ctx.data().db;
    let user =
        UserService::get_or_create(db, ctx.author().id.get() as i64, &ctx.author().name).await?;

    let username = user.username.clone().unwrap_or_default();
    let mut embed = serenity::CreateEmbed::new()
        .title(format!("👤 {}'s Profile", username))
        .colour(colour::blue())
        .field("🏆 Rank", user.get_rank(), true)
        .field("📊 Level", user.level.to_string(), true)
        .field("⭐ Points", user.sustainability_points.to_string(), true)
        .field("📝 Queries", user.queries_count.to_string(), true);

    let points_to_next = user.level * 100 - user.sustainability_points;
    if points_to_next > 0 {
        embed = embed.field(
            "📈 Progress",
            format!(
                "{} points to level {}",
                points_to_next,
                user.level + 1
            ),
            true,
        );
    }
    if let Some(pm) = &user.preferred_materials {
        embed = embed.field("Preferred Materials", pm, false);
    }
    if let Some(p) = &user.sustainability_priority {
        embed = embed.field("Priority", title_case(p), true);
    }
    if let Some(b) = &user.budget_range {
        embed = embed.field("Budget Range", b, true);
    }
    embed = embed
        .thumbnail(ctx.author().face())
        .footer(serenity::CreateEmbedFooter::new(
            "Keep learning about sustainability to earn more points!",
        ));
    send_embed(&ctx, embed).await
}

/// View the sustainability leaderboard.
#[poise::command(prefix_command, aliases("lb", "top"))]
pub async fn leaderboard(ctx: Context<'_>) -> Result<(), Error> {
    typing(&ctx).await;
    let db = &ctx.data().db;
    let top = UserService::get_leaderboard(db, 10).await?;
    if top.is_empty() {
        return say(&ctx, "❌ No users found on leaderboard.").await;
    }

    let medals = ["🥇", "🥈", "🥉"];
    let mut embed = serenity::CreateEmbed::new()
        .title("🏆 Sustainability Champions Leaderboard")
        .description("Top users making a difference:")
        .colour(colour::gold());

    for (i, u) in top.iter().enumerate() {
        let medal = if i < 3 {
            medals[i].to_string()
        } else {
            format!("**{}.**", i + 1)
        };
        embed = embed.field(
            format!("{} {}", medal, u.username.clone().unwrap_or_default()),
            format!(
                "Level {} • {} points • {}",
                u.level,
                u.sustainability_points,
                u.get_rank()
            ),
            false,
        );
    }

    let me = UserService::get_by_discord_id(db, ctx.author().id.get() as i64).await?;
    if let Some(cur) = me {
        if !top.iter().any(|u| u.id == cur.id) {
            embed = embed.field(
                "\nYour Stats",
                format!(
                    "Level {} • {} points",
                    cur.level, cur.sustainability_points
                ),
                false,
            );
        }
    }
    embed = embed.footer(serenity::CreateEmbedFooter::new(
        "Use !profile to view your complete profile",
    ));
    send_embed(&ctx, embed).await
}

/// Set your sustainability preferences.
#[poise::command(prefix_command, aliases("pref"))]
pub async fn setpreference(
    ctx: Context<'_>,
    #[description = "materials | budget | priority"] preference_type: String,
    #[rest]
    #[description = "Value"]
    value: String,
) -> Result<(), Error> {
    let db = &ctx.data().db;
    let mut user =
        UserService::get_or_create(db, ctx.author().id.get() as i64, &ctx.author().name).await?;

    match preference_type.to_lowercase().as_str() {
        "materials" | "material" => {
            user.preferred_materials = Some(value.clone());
            say(&ctx, format!("✅ Preferred materials set to: {value}")).await?;
        }
        "budget" => {
            if !matches!(value.as_str(), "$" | "$$" | "$$$" | "$$$$") {
                return say(&ctx, "❌ Budget must be: $, $$, $$$, or $$$$").await;
            }
            user.budget_range = Some(value.clone());
            say(&ctx, format!("✅ Budget range set to: {value}")).await?;
        }
        "priority" => {
            let valid = ["environmental", "social", "animal_welfare", "all"];
            let lower = value.to_lowercase();
            if !valid.contains(&lower.as_str()) {
                return say(
                    &ctx,
                    format!("❌ Priority must be one of: {}", valid.join(", ")),
                )
                .await;
            }
            user.sustainability_priority = Some(lower);
            say(&ctx, format!("✅ Sustainability priority set to: {value}")).await?;
        }
        _ => {
            return say(
                &ctx,
                "❌ Invalid preference type. Use: materials, budget, or priority",
            )
            .await
        }
    }

    UserService::update(db, &user).await?;
    Ok(())
}
