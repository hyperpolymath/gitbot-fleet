# SPDX-License-Identifier: MIT OR Apache-2.0
# SPDX-FileCopyrightText: 2025 hyperpolymath

FROM rust:1.85-slim-bookworm AS builder

WORKDIR /build

COPY Cargo.toml Cargo.lock* ./
COPY src/ src/
COPY crates/ crates/

RUN cargo build --release --bin hotchocolabot

FROM debian:bookworm-slim

RUN apt-get update && apt-get install -y \
    ca-certificates \
    && rm -rf /var/lib/apt/lists/*

COPY --from=builder /build/target/release/hotchocolabot /usr/local/bin/hotchocolabot

ENTRYPOINT ["hotchocolabot"]
CMD ["--help"]
