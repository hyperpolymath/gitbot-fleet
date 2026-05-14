# SPDX-License-Identifier: PMPL-1.0-or-later
# Multi-stage build for fleet-dashboard

FROM rust:1.75-slim AS builder

WORKDIR /build

# Install build dependencies
RUN apt-get update && apt-get install -y \
    pkg-config \
    libssl-dev \
    && rm -rf /var/lib/apt/lists/*

# Copy workspace files
COPY shared-context ../shared-context
COPY dashboard .

# Build release binary
RUN cargo build --release --bin fleet-dashboard

# Runtime stage
FROM debian:bookworm-slim

# Install runtime dependencies
RUN apt-get update && apt-get install -y \
    ca-certificates \
    libssl3 \
    && rm -rf /var/lib/apt/lists/*

# Create non-root user
RUN useradd -m -u 1000 fleet && \
    mkdir -p /app/static && \
    chown -R fleet:fleet /app

USER fleet
WORKDIR /app

# Copy binary and static files
COPY --from=builder /build/target/release/fleet-dashboard /app/
COPY --chown=fleet:fleet dashboard/static /app/static

# Environment defaults
ENV FLEET_REPO_PATH=/repo
ENV FLEET_REPO_NAME=monitored-repo
ENV RUST_LOG=fleet_dashboard=info

# Expose dashboard port
EXPOSE 8080

# Health check
HEALTHCHECK --interval=30s --timeout=3s --start-period=5s --retries=3 \
    CMD curl -f http://localhost:8080/api/health || exit 1

CMD ["/app/fleet-dashboard"]
