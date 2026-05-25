// SPDX-License-Identifier: MPL-2.0
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
//! Secure execution environment for prover verification

pub mod container;

pub use container::{ExecutionResult, IsolationBackend, PodmanExecutor};
