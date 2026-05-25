// SPDX-License-Identifier: MPL-2.0
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
//! API layer - GraphQL and webhook handlers

pub mod graphql;
pub mod webhooks;

pub use graphql::create_schema;
pub use webhooks::webhook_router;
