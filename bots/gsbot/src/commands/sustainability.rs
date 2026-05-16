// SPDX-License-Identifier: PMPL-1.0-or-later
//! Sustainability commands cog: `sustainability`, `alternatives`, `care`,
//! `tips`.

use poise::serenity_prelude as serenity;
use rand::seq::SliceRandom;

use super::{colour, say, send_embed};
use crate::services::{GarmentService, UserService};
use crate::sustainability as analyzer;
use crate::{typing, Context, Error};

/// Python `str(float)` parity (e.g. 3.0 -> "3.0", 1.5 -> "1.5").
fn pyf(x: f64) -> String {
    format!("{x:?}")
}

async fn award(ctx: &Context<'_>, points: i64) -> Result<(), Error> {
    let db = &ctx.data().db;
    let mut user =
        UserService::get_or_create(db, ctx.author().id.get() as i64, &ctx.author().name).await?;
    user.add_points(points);
    UserService::update(db, &user).await?;
    Ok(())
}

/// Get sustainability score for a garment.
#[poise::command(prefix_command, aliases("sus", "score"))]
pub async fn sustainability(
    ctx: Context<'_>,
    #[rest]
    #[description = "Garment name"]
    garment_name: String,
) -> Result<(), Error> {
    typing(&ctx).await;
    award(&ctx, 5).await?;
    let db = &ctx.data().db;

    let garment = match GarmentService::get_by_name(db, &garment_name).await? {
        Some(g) => g,
        None => {
            return say(
                &ctx,
                format!(
                    "❌ Garment '{garment_name}' not found. Try `!search {garment_name}` \
                     to find similar items."
                ),
            )
            .await
        }
    };

    let materials = GarmentService::materials_of(db, garment.id).await?;
    let score = garment.calculate_sustainability_score(&materials);
    let impact = garment.get_environmental_impact(&materials);
    let category = analyzer::get_impact_category(score);

    let mut embed = serenity::CreateEmbed::new()
        .title(format!("🌱 {}", garment.name))
        .description(
            garment
                .description
                .clone()
                .unwrap_or_else(|| "No description available".into()),
        )
        .colour(if score >= 60.0 {
            colour::green()
        } else {
            colour::orange()
        })
        .field(
            "Sustainability Score",
            format!("**{score:.1}/100** - {category}"),
            false,
        )
        .field("💧 Water Usage", &impact.water_usage, true)
        .field("🌍 Carbon Footprint", &impact.carbon_footprint, true)
        .field("⚡ Energy", &impact.energy_consumption, true);

    if !materials.is_empty() {
        let text = materials
            .iter()
            .map(|m| format!("• {} ({})", m.name, m.material_type))
            .collect::<Vec<_>>()
            .join("\n");
        embed = embed.field("Materials", text, false);
    }
    if let Some(yrs) = garment.expected_lifespan_years {
        embed = embed.field("Expected Lifespan", format!("{} years", pyf(yrs)), true);
    }
    embed = embed.footer(serenity::CreateEmbedFooter::new(format!(
        "Requested by {} | +5 points",
        ctx.author().name
    )));

    send_embed(&ctx, embed).await
}

/// Find sustainable alternatives for a garment.
#[poise::command(prefix_command, aliases("alt"))]
pub async fn alternatives(
    ctx: Context<'_>,
    #[rest]
    #[description = "Garment name"]
    garment_name: String,
) -> Result<(), Error> {
    typing(&ctx).await;
    award(&ctx, 5).await?;
    let db = &ctx.data().db;

    let garment = match GarmentService::get_by_name(db, &garment_name).await? {
        Some(g) => g,
        None => return say(&ctx, format!("❌ Garment '{garment_name}' not found.")).await,
    };

    let materials = GarmentService::materials_of(db, garment.id).await?;
    let current = garment.calculate_sustainability_score(&materials);
    let alternatives = GarmentService::get_alternatives(db, &garment, current).await?;

    if alternatives.is_empty() {
        return say(
            &ctx,
            format!(
                "✅ Great news! '{}' is already one of the most sustainable options \
                 in its category.",
                garment.name
            ),
        )
        .await;
    }

    let mut embed = serenity::CreateEmbed::new()
        .title(format!("♻️ Sustainable Alternatives to {}", garment.name))
        .description(format!(
            "Here are more sustainable options in the {} category:",
            garment.category
        ))
        .colour(colour::green());

    for alt in alternatives.iter().take(5) {
        let alt_mats = GarmentService::materials_of(db, alt.id).await?;
        let alt_score = alt.calculate_sustainability_score(&alt_mats);
        embed = embed.field(
            format!("{} - Score: {alt_score:.1}/100", alt.name),
            alt.description
                .clone()
                .unwrap_or_else(|| "No description".into()),
            false,
        );
    }
    embed = embed.footer(serenity::CreateEmbedFooter::new(format!(
        "Requested by {} | +5 points",
        ctx.author().name
    )));
    send_embed(&ctx, embed).await
}

/// Get care instructions to extend garment life.
#[poise::command(prefix_command)]
pub async fn care(
    ctx: Context<'_>,
    #[rest]
    #[description = "Garment name"]
    garment_name: String,
) -> Result<(), Error> {
    typing(&ctx).await;
    award(&ctx, 3).await?;
    let db = &ctx.data().db;

    let garment = match GarmentService::get_by_name(db, &garment_name).await? {
        Some(g) => g,
        None => return say(&ctx, format!("❌ Garment '{garment_name}' not found.")).await,
    };

    let materials = GarmentService::materials_of(db, garment.id).await?;
    let names: Vec<String> = materials.iter().map(|m| m.name.clone()).collect();
    let tips = analyzer::generate_care_tips(&names);

    let mut embed = serenity::CreateEmbed::new()
        .title(format!("🧺 Care Instructions: {}", garment.name))
        .description("Follow these tips to extend your garment's life:")
        .colour(colour::blue());

    if let Some(ci) = &garment.care_instructions {
        embed = embed.field("Specific Care", ci, false);
    }
    let tips_text = tips
        .iter()
        .take(8)
        .map(|t| format!("• {t}"))
        .collect::<Vec<_>>()
        .join("\n");
    embed = embed.field("General Tips", tips_text, false);
    if let Some(wf) = &garment.washing_frequency {
        embed = embed.field("Recommended Washing", wf, false);
    }
    embed = embed.footer(serenity::CreateEmbedFooter::new(format!(
        "Requested by {} | +3 points",
        ctx.author().name
    )));
    send_embed(&ctx, embed).await
}

/// Get daily sustainability tips.
#[poise::command(prefix_command)]
pub async fn tips(ctx: Context<'_>) -> Result<(), Error> {
    typing(&ctx).await;
    award(&ctx, 2).await?;

    let all = analyzer::get_sustainability_tips();
    let n = std::cmp::min(5, all.len());
    let selected: Vec<&&str> = all.choose_multiple(&mut rand::thread_rng(), n).collect();

    let body = selected
        .iter()
        .enumerate()
        .map(|(i, t)| format!("**{}.** {}", i + 1, t))
        .collect::<Vec<_>>()
        .join("\n\n");

    let embed = serenity::CreateEmbed::new()
        .title("🌍 Sustainability Tips")
        .description("Here are some tips for sustainable fashion:")
        .colour(colour::green())
        .field("Tips", body, false)
        .footer(serenity::CreateEmbedFooter::new(format!(
            "Requested by {} | +2 points",
            ctx.author().name
        )));
    send_embed(&ctx, embed).await
}
