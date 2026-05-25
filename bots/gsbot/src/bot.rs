// SPDX-License-Identifier: MPL-2.0
//! Bot wiring — poise/serenity equivalent of `bot/bot.py`: prefix, intents,
//! presence, cog loading, and the command-error message mapping.

use anyhow::Result;
use poise::serenity_prelude as serenity;

use gsbot::{commands, config::Config, Data, Error};

/// Map framework errors to the same user-facing messages as the Python
/// `on_command_error` handler.
async fn on_error(error: poise::FrameworkError<'_, Data, Error>) {
    use poise::FrameworkError as FE;
    match error {
        FE::UnknownCommand {
            ctx, msg, prefix, ..
        } => {
            let _ = msg
                .channel_id
                .say(
                    &ctx.http,
                    format!(
                        "❌ Command not found. Use `{prefix}help` to see available commands."
                    ),
                )
                .await;
        }
        FE::ArgumentParse { ctx, input, .. } => {
            let what = input.unwrap_or_default();
            let _ = ctx
                .say(format!("❌ Missing required argument: {what}"))
                .await;
        }
        FE::CommandCheckFailed { ctx, .. } => {
            let _ = ctx
                .say("❌ You don't have permission to use this command.")
                .await;
        }
        FE::Command { error, ctx, .. } => {
            tracing::error!("Command error: {error:?}");
            let _ = ctx
                .say("❌ An error occurred while processing the command.")
                .await;
        }
        other => {
            if let Err(e) = poise::builtins::on_error(other).await {
                tracing::error!("Error in error handler: {e:?}");
            }
        }
    }
}

/// Build and run the bot. Equivalent to `create_bot()` + `bot.start()`.
pub async fn run(config: Config, db: sqlx::SqlitePool) -> Result<()> {
    let token = config.discord_token.clone();
    let prefix = config.discord_prefix.clone();
    let presence = format!("sustainable fashion | {prefix}help");

    let options = poise::FrameworkOptions {
        commands: commands::all(),
        prefix_options: poise::PrefixFrameworkOptions {
            prefix: Some(prefix),
            ..Default::default()
        },
        on_error: |e| Box::pin(on_error(e)),
        ..Default::default()
    };

    let framework = poise::Framework::builder()
        .options(options)
        .setup(move |ctx, ready, _framework| {
            Box::pin(async move {
                tracing::info!(
                    "Logged in as {} ({})",
                    ready.user.name,
                    ready.user.id
                );
                ctx.set_activity(Some(serenity::ActivityData::watching(presence)));
                Ok(Data { db, config })
            })
        })
        .build();

    let intents = serenity::GatewayIntents::GUILDS
        | serenity::GatewayIntents::GUILD_MESSAGES
        | serenity::GatewayIntents::MESSAGE_CONTENT
        | serenity::GatewayIntents::GUILD_MEMBERS;

    let mut client = serenity::ClientBuilder::new(token, intents)
        .framework(framework)
        .await?;

    tracing::info!("Starting Garment Sustainability Bot...");
    client.start().await?;
    Ok(())
}
