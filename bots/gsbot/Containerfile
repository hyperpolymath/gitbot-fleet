# SPDX-License-Identifier: PMPL-1.0-or-later
# Garment Sustainability Bot — Rust/SPARK build (fleet convention:
# rust builder + debian-slim runtime, non-root).

FROM docker.io/library/rust:latest AS builder

WORKDIR /build

RUN apt-get update && \
    apt-get install -y pkg-config && \
    rm -rf /var/lib/apt/lists/*

# migrations/ is embedded at compile time via sqlx::migrate!.
COPY Cargo.toml Cargo.lock ./
COPY migrations ./migrations
COPY src ./src

RUN cargo build --release --bin gsbot

# Runtime image
FROM docker.io/library/debian:bookworm-slim

RUN apt-get update && \
    apt-get install -y ca-certificates && \
    rm -rf /var/lib/apt/lists/*

WORKDIR /app
COPY --from=builder /build/target/release/gsbot /usr/local/bin/gsbot

RUN useradd -r -s /bin/false gsbot && \
    mkdir -p /app/data /app/logs && \
    chown -R gsbot:gsbot /app
USER gsbot

VOLUME ["/app/data"]

ENTRYPOINT ["/usr/local/bin/gsbot"]
