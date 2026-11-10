# SPDX-License-Identifier: MPL-2.0
# Override WOLFI_IMAGE with cgr.dev/chainguard/wolfi-base@sha256:<digest>
# for a reviewed, immutable release build. Never invent an unverified digest.
ARG WOLFI_IMAGE=cgr.dev/chainguard/wolfi-base:latest
FROM ${WOLFI_IMAGE} AS development

RUN apk add --no-cache bash ca-certificates curl git build-base cmake \
    pkgconf sqlite-dev tar gzip xz coreutils libstdc++

ENV MISE_DATA_DIR=/opt/mise \
    MISE_CONFIG_DIR=/opt/mise-config \
    MISE_CACHE_DIR=/tmp/mise-cache \
    RUSTUP_HOME=/opt/rustup \
    CARGO_HOME=/opt/cargo \
    MISE_BIN_DIR=/usr/local/bin
ENV PATH="/opt/cargo/bin:/opt/mise/shims:${PATH}"
WORKDIR /workspace
COPY mise.toml rust-toolchain.toml ./
COPY scripts/bootstrap.sh scripts/bootstrap.sh
RUN bash scripts/bootstrap.sh \
    && ln -s "$(mise which just)" /usr/local/bin/just \
    && chmod -R a+rX /opt/rustup /opt/cargo /opt/mise \
    && adduser -D -u 1000 dev \
    && mkdir -p /home/dev/.cargo /workspace/target \
    && chown -R dev:dev /home/dev /workspace

ENV CARGO_HOME=/home/dev/.cargo HOME=/home/dev
USER dev
CMD ["bash"]

FROM development AS builder
COPY --chown=dev:dev . .
RUN cargo build --locked --release

FROM ${WOLFI_IMAGE} AS runtime
LABEL org.opencontainers.image.source="https://github.com/hyperpolymath/echidnabot"
LABEL org.opencontainers.image.description="Proof-aware CI bot that orchestrates ECHIDNA for theorem proof verification"
LABEL org.opencontainers.image.licenses="MPL-2.0"
RUN apk add --no-cache sqlite-libs ca-certificates libstdc++ \
    && adduser -D -u 1000 echidna
COPY --from=builder /workspace/target/release/echidnabot /usr/local/bin/echidnabot
USER echidna
WORKDIR /home/echidna
EXPOSE 8080
ENTRYPOINT ["/usr/local/bin/echidnabot"]
