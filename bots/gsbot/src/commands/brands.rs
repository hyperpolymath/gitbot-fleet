// SPDX-License-Identifier: PMPL-1.0-or-later
//! Brands cog: `brands`.

use poise::serenity_prelude as serenity;

use super::{colour, say, send_embed};
use crate::services::{BrandService, UserService};
use crate::{typing, Context, Error};

/// Get information about sustainable brands, or detail for one brand.
#[poise::command(prefix_command, aliases("brand"))]
pub async fn brands(
    ctx: Context<'_>,
    #[rest]
    #[description = "Brand name (optional)"]
    brand_name: Option<String>,
) -> Result<(), Error> {
    typing(&ctx).await;
    let db = &ctx.data().db;

    let mut user =
        UserService::get_or_create(db, ctx.author().id.get() as i64, &ctx.author().name).await?;
    user.add_points(5);
    UserService::update(db, &user).await?;

    match brand_name {
        Some(name) => {
            let brand = match BrandService::get_by_name(db, &name).await? {
                Some(b) => b,
                None => return say(&ctx, format!("❌ Brand '{name}' not found.")).await,
            };

            let mut embed = serenity::CreateEmbed::new()
                .title(format!("🏷️ {}", brand.name))
                .description(
                    brand
                        .description
                        .clone()
                        .unwrap_or_else(|| "No description available".into()),
                )
                .colour(if brand.overall_rating >= 60.0 {
                    colour::green()
                } else {
                    colour::orange()
                });
            if let Some(url) = brand.website.clone() {
                embed = embed.url(url);
            }
            embed = embed
                .field(
                    "Overall Rating",
                    format!(
                        "**{:.1}/100**\n{}",
                        brand.overall_rating,
                        brand.get_rating_summary()
                    ),
                    false,
                )
                .field(
                    "🌍 Environmental",
                    format!("{:.1}/100", brand.environmental_rating),
                    true,
                )
                .field("👥 Labor", format!("{:.1}/100", brand.labor_rating), true)
                .field(
                    "🐾 Animal Welfare",
                    format!("{:.1}/100", brand.animal_welfare_rating),
                    true,
                );

            let badges = brand.get_certification_badges();
            if !badges.is_empty() {
                embed = embed.field(
                    "Certifications",
                    badges
                        .iter()
                        .map(|b| format!("✓ {b}"))
                        .collect::<Vec<_>>()
                        .join("\n"),
                    false,
                );
            }
            if let Some(c) = &brand.country {
                embed = embed.field("Country", c, true);
            }
            if let Some(p) = &brand.price_range {
                embed = embed.field("Price Range", p, true);
            }
            if let Some(t) = brand.transparency_score {
                embed = embed.field("Transparency", format!("{t:.1}/100"), true);
            }
            if let Some(g) = &brand.good_on_you_rating {
                embed = embed.field("Good On You Rating", g, false);
            }
            embed = embed.footer(serenity::CreateEmbedFooter::new(format!(
                "Requested by {} | +5 points",
                ctx.author().name
            )));
            send_embed(&ctx, embed).await
        }
        None => {
            let brands = BrandService::get_top_rated(db, 10).await?;
            if brands.is_empty() {
                return say(&ctx, "❌ No brands found in database.").await;
            }
            let mut embed = serenity::CreateEmbed::new()
                .title("🌟 Top Sustainable Brands")
                .description("Here are the highest-rated sustainable fashion brands:")
                .colour(colour::gold());
            for (i, b) in brands.iter().enumerate() {
                embed = embed.field(
                    format!("{}. {} - {:.1}/100", i + 1, b.name, b.overall_rating),
                    b.get_rating_summary(),
                    false,
                );
            }
            embed = embed.footer(serenity::CreateEmbedFooter::new(
                "Use !brands <name> for detailed info | +5 points",
            ));
            send_embed(&ctx, embed).await
        }
    }
}
