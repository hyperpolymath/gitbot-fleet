// SPDX-License-Identifier: MPL-2.0
//! Materials cog: `impact`, `compare`, `search`.

use poise::serenity_prelude as serenity;

use super::{colour, say, send_embed};
use crate::models::MaterialType;
use crate::services::{GarmentService, MaterialService, UserService};
use crate::sustainability as analyzer;
use crate::{typing, Context, Error};

async fn award(ctx: &Context<'_>, points: i64) -> Result<(), Error> {
    let db = &ctx.data().db;
    let mut user =
        UserService::get_or_create(db, ctx.author().id.get() as i64, &ctx.author().name).await?;
    user.add_points(points);
    UserService::update(db, &user).await?;
    Ok(())
}

/// Get environmental impact of a material.
#[poise::command(prefix_command, aliases("material"))]
pub async fn impact(
    ctx: Context<'_>,
    #[rest]
    #[description = "Material name"]
    material_name: String,
) -> Result<(), Error> {
    typing(&ctx).await;
    award(&ctx, 5).await?;
    let db = &ctx.data().db;

    let material = match MaterialService::get_by_name(db, &material_name).await? {
        Some(m) => m,
        None => return say(&ctx, format!("❌ Material '{material_name}' not found.")).await,
    };

    let overall = material.calculate_overall_score();
    let grade = material.get_grade();
    // NB: the original Python computed an impact "category" here but never
    // rendered it in the embed; omitted to keep output identical.
    let recommendation = analyzer::get_material_recommendation(&material);
    let type_display = material.type_display();

    let mut embed = serenity::CreateEmbed::new()
        .title(format!("🧵 {}", material.name))
        .description(
            material
                .description
                .clone()
                .unwrap_or_else(|| "No description available".into()),
        )
        .colour(if overall >= 60.0 {
            colour::green()
        } else {
            colour::orange()
        })
        .field(
            "Overall Score",
            format!("**{overall:.1}/100** (Grade: {grade})"),
            false,
        )
        .field("Type", type_display, true)
        .field(
            "💧 Water Usage",
            format!("{:.1}/100", material.water_usage_score),
            true,
        )
        .field(
            "🌍 Carbon Footprint",
            format!("{:.1}/100", material.carbon_footprint_score),
            true,
        )
        .field(
            "♻️ Biodegradability",
            format!("{:.1}/100", material.biodegradability_score),
            true,
        )
        .field(
            "🧪 Chemical Usage",
            format!("{:.1}/100", material.chemical_usage_score),
            true,
        )
        .field(
            "⚡ Energy Consumption",
            format!("{:.1}/100", material.energy_consumption_score),
            true,
        );

    if let Some(w) = material.water_liters_per_kg {
        embed = embed.field("Water per kg", format!("{w:.0} L"), true);
    }
    if let Some(c) = material.co2_kg_per_kg {
        embed = embed.field("CO2 per kg", format!("{c:.2} kg"), true);
    }
    if let Some(b) = &material.is_biodegradable {
        embed = embed.field("Biodegradable", b, true);
    }
    if let Some(r) = &material.recycling_potential {
        embed = embed.field("Recycling Potential", r, true);
    }
    embed = embed.field("Recommendation", recommendation, false);
    embed = embed.footer(serenity::CreateEmbedFooter::new(format!(
        "Requested by {} | +5 points",
        ctx.author().name
    )));
    send_embed(&ctx, embed).await
}

/// Compare two materials.
#[poise::command(prefix_command)]
pub async fn compare(
    ctx: Context<'_>,
    #[description = "First material"] material1: String,
    #[description = "Second material"] material2: String,
) -> Result<(), Error> {
    typing(&ctx).await;
    award(&ctx, 7).await?;
    let db = &ctx.data().db;

    let mat1 = match MaterialService::get_by_name(db, &material1).await? {
        Some(m) => m,
        None => return say(&ctx, format!("❌ Material '{material1}' not found.")).await,
    };
    let mat2 = match MaterialService::get_by_name(db, &material2).await? {
        Some(m) => m,
        None => return say(&ctx, format!("❌ Material '{material2}' not found.")).await,
    };

    let cmp = analyzer::compare_materials(&mat1, &mat2);
    let winner = if cmp.overall1 > cmp.overall2 {
        &cmp.material1
    } else {
        &cmp.material2
    };

    let mut embed = serenity::CreateEmbed::new()
        .title(format!("⚖️ {} vs {}", mat1.name, mat2.name))
        .description("Material Comparison")
        .colour(colour::blue())
        .field(
            "Overall Scores",
            format!(
                "**{}**: {:.1}/100\n**{}**: {:.1}/100\n\n🏆 **Winner**: {}",
                mat1.name, cmp.overall1, mat2.name, cmp.overall2, winner
            ),
            false,
        );

    for (label, m) in [
        ("💧 Water Usage", &cmp.water_usage),
        ("🌍 Carbon Footprint", &cmp.carbon_footprint),
        ("♻️ Biodegradability", &cmp.biodegradability),
    ] {
        embed = embed.field(
            label,
            format!(
                "{}: {:.1}\n{}: {:.1}\n✓ {}",
                mat1.name, m.a, mat2.name, m.b, m.better
            ),
            true,
        );
    }
    embed = embed.footer(serenity::CreateEmbedFooter::new(format!(
        "Requested by {} | +7 points",
        ctx.author().name
    )));
    send_embed(&ctx, embed).await
}

/// Search for garments or materials.
#[poise::command(prefix_command)]
pub async fn search(
    ctx: Context<'_>,
    #[rest]
    #[description = "Search query"]
    query: String,
) -> Result<(), Error> {
    typing(&ctx).await;
    let db = &ctx.data().db;

    let materials = MaterialService::search(db, &query).await?;
    let garments = GarmentService::search(db, &query).await?;

    if materials.is_empty() && garments.is_empty() {
        return say(&ctx, format!("❌ No results found for '{query}'.")).await;
    }

    let mut embed = serenity::CreateEmbed::new()
        .title(format!("🔍 Search Results for '{query}'"))
        .colour(colour::blue());

    if !materials.is_empty() {
        let text = materials
            .iter()
            .take(5)
            .map(|m| {
                let ty = MaterialType::parse(&m.material_type)
                    .map(|t| t.value())
                    .unwrap_or(m.material_type.as_str());
                format!("• {} ({})", m.name, ty)
            })
            .collect::<Vec<_>>()
            .join("\n");
        embed = embed.field(format!("Materials ({})", materials.len()), text, false);
    }
    if !garments.is_empty() {
        let text = garments
            .iter()
            .take(5)
            .map(|g| format!("• {} ({})", g.name, g.category))
            .collect::<Vec<_>>()
            .join("\n");
        embed = embed.field(format!("Garments ({})", garments.len()), text, false);
    }
    embed = embed.footer(serenity::CreateEmbedFooter::new(format!(
        "Requested by {}",
        ctx.author().name
    )));
    send_embed(&ctx, embed).await
}
