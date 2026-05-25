// SPDX-License-Identifier: MPL-2.0
//! Command surface — faithful port of the discord.py cogs. Each former
//! cog is a submodule; command names, aliases, argument shapes, point
//! awards and embed fields are preserved.

pub mod admin;
pub mod brands;
pub mod materials;
pub mod sustainability;
pub mod user_commands;

use poise::serenity_prelude as serenity;

use crate::{Context, Data, Error};

/// discord.py colour parity (explicit hex from `discord.Color`).
pub mod colour {
    use poise::serenity_prelude::Colour;
    pub fn green() -> Colour {
        Colour::new(0x2ECC71)
    }
    pub fn orange() -> Colour {
        Colour::new(0xE67E22)
    }
    pub fn blue() -> Colour {
        Colour::new(0x3498DB)
    }
    pub fn gold() -> Colour {
        Colour::new(0xF1C40F)
    }
}

/// Send a single embed (the common `await ctx.send(embed=...)` shape).
pub async fn send_embed(ctx: &Context<'_>, embed: serenity::CreateEmbed) -> Result<(), Error> {
    ctx.send(poise::CreateReply::default().embed(embed)).await?;
    Ok(())
}

/// Plain text reply (the `await ctx.send("…")` shape).
pub async fn say(ctx: &Context<'_>, msg: impl Into<String>) -> Result<(), Error> {
    ctx.say(msg.into()).await?;
    Ok(())
}

/// Admin gate. Mirrors `is_admin()`: author in `DISCORD_ADMIN_IDS`, or
/// the invoking member has the Administrator permission.
pub async fn is_admin(ctx: Context<'_>) -> Result<bool, Error> {
    let ids = &ctx.data().config.discord_admin_ids;
    if ids.contains(&(ctx.author().id.get() as i64)) {
        return Ok(true);
    }
    // Administrator permission check. `Member::permissions` is deprecated
    // (ignores channel overwrites) but server-wide Administrator is exactly
    // the guild-level bit the original `ctx.author.guild_permissions
    // .administrator` test used, so it is the faithful equivalent here.
    #[allow(deprecated)]
    if let Some(member) = ctx.author_member().await {
        if let Ok(perms) = member.permissions(ctx.cache()) {
            return Ok(perms.administrator());
        }
    }
    Ok(false)
}

/// All commands, for `FrameworkOptions`.
pub fn all() -> Vec<poise::Command<Data, Error>> {
    vec![
        sustainability::sustainability(),
        sustainability::alternatives(),
        sustainability::care(),
        sustainability::tips(),
        materials::impact(),
        materials::compare(),
        materials::search(),
        brands::brands(),
        user_commands::profile(),
        user_commands::leaderboard(),
        user_commands::setpreference(),
        admin::loaddata(),
        admin::stats(),
        admin::announce(),
    ]
}
