// SPDX-License-Identifier: MPL-2.0
// Copyright (c) Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
//! API layer - GraphQL and webhook handlers

pub mod graphql;
pub mod rate_limit;
pub mod webhooks;

pub use graphql::create_schema;
pub use webhooks::{webhook_router, AppState as WebhookAppState};
