# SPDX-License-Identifier: PMPL-1.0-or-later
# Vordr - The Guardian
# Norse mythology: watcher/guardian
# Purpose: Seam hygiene and architectural boundary monitoring

FROM docker.io/library/rust:latest AS builder

WORKDIR /build

# Install dependencies and nightly toolchain
RUN apt-get update && \
    apt-get install -y pkg-config libssl-dev && \
    rm -rf /var/lib/apt/lists/* && \
    rustup default nightly

# Copy source
COPY Cargo.toml Cargo.lock ./
COPY src ./src

# Build release binary
RUN cargo build --release --bin seambot

# Runtime image
FROM docker.io/library/debian:bookworm-slim

RUN apt-get update && \
    apt-get install -y ca-certificates libssl3 && \
    rm -rf /var/lib/apt/lists/*

COPY --from=builder /build/target/release/seambot /usr/local/bin/vordr

# Run as non-root
RUN useradd -r -s /bin/false vordr
USER vordr

ENTRYPOINT ["/usr/local/bin/vordr"]
CMD ["check"]
