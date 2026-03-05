# SPDX-License-Identifier: PMPL-1.0-or-later
# Multi-stage build for fleet CLI

FROM rust:1.75-slim AS builder

WORKDIR /build

# Install build dependencies
RUN apt-get update && apt-get install -y \
    pkg-config \
    libssl-dev \
    && rm -rf /var/lib/apt/lists/*

# Copy workspace files
COPY shared-context .

# Build release binary
RUN cd fleet-cli && cargo build --release

# Runtime stage
FROM debian:bookworm-slim

# Install runtime dependencies
RUN apt-get update && apt-get install -y \
    ca-certificates \
    libssl3 \
    && rm -rf /var/lib/apt/lists/*

# Create non-root user
RUN useradd -m -u 1000 fleet

USER fleet
WORKDIR /home/fleet

# Copy binary
COPY --from=builder /build/fleet-cli/target/release/fleet /usr/local/bin/

ENTRYPOINT ["fleet"]
CMD ["--help"]
