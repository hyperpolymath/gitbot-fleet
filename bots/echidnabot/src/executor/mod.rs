// SPDX-License-Identifier: MPL-2.0
// Copyright (c) Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
//! Secure execution environment for prover verification

pub mod container;

pub use container::{ExecutionResult, IsolationBackend, PodmanExecutor};
