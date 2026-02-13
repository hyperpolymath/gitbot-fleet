# Cipherbot — Sonnet Task Plan (NEW BOT)

## Context

Cipherbot (name avoids "securi-" prefix per user request) is a NEW specialized bot in the gitbot-fleet ecosystem focused on **cryptographic hygiene, protocol compliance, and post-quantum readiness**. It operates in all 4 modes: Consultant, Regulator, Advisor, and Policy enforcer.

**This bot does not yet exist.** This plan describes creating it from scratch using the RSR template.

**Philosophy**: Proactive cryptographic attestation. No MD5, no SHA1, no SHA-256 alone. Post-quantum readiness. Formal verification of crypto primitives where possible.

**User's security standards (MANDATORY)**:
- Password Hashing: Argon2id (512 MiB, 8 iter, 4 lanes)
- General Hashing: SHAKE3-512 (FIPS 202)
- PQ Signatures: Dilithium5-AES hybrid (ML-DSA-87, FIPS 204)
- PQ Key Exchange: Kyber-1024 + SHAKE256-KDF (ML-KEM-1024, FIPS 203)
- Classical Sigs: Ed448 + Dilithium5 hybrid
- Symmetric: XChaCha20-Poly1305 (256-bit key)
- KDF: HKDF-SHAKE512 (FIPS 202)
- RNG: ChaCha20-DRBG (512-bit seed, SP 800-90Ar1)
- Database Hashing: BLAKE3 (512-bit) + SHAKE3-512
- Protocol: QUIC + HTTP/3 + IPv6 (IPv4 disabled)
- Fallback: SPHINCS+ for all hybrid PQ systems
- Formal Verification: Coq/Isabelle for crypto primitives

---

## Task 0: Scaffold the Repository

### 0.1 Clone from RSR template
```bash
cd ~/Documents/hyperpolymath-repos
git clone https://github.com/hyperpolymath/rsr-template-repo cipherbot
cd cipherbot
rm -rf .git && git init -b main
```

### 0.2 Set up Rust project
```toml
[package]
name = "cipherbot"
version = "0.1.0"
edition = "2021"
license = "PMPL-1.0-or-later"
authors = ["Jonathan D.A. Jewell <jonathan.jewell@open.ac.uk>"]
description = "Cryptographic Hygiene & Post-Quantum Readiness Bot for gitbot-fleet"

[dependencies]
gitbot-shared-context = { path = "../gitbot-fleet/shared-context" }
clap = { version = "4", features = ["derive"] }
serde = { version = "1", features = ["derive"] }
serde_json = "1"
anyhow = "1"
tracing = "0.1"
tracing-subscriber = "0.3"
walkdir = "2"
regex = "1"
tree-sitter = "0.24"
tree-sitter-rust = "0.23"
tree-sitter-javascript = "0.23"
```

---

## Task 1: Deprecated Crypto Detection Analyzers

### 1.1 Hash Function Analyzer (`src/analyzers/hashing.rs`)

Detect usage of deprecated/weak hash functions:

| Status | Algorithm | Action |
|--------|-----------|--------|
| REJECT | MD5 | Error — broken, no use ever |
| REJECT | SHA-1 | Error — broken, terminate immediately |
| WARN | SHA-256 alone | Warning — use SHAKE3-512 or BLAKE3 |
| WARN | SHA-384 | Warning — prefer SHAKE3-512 |
| ACCEPT | SHA-512 | Note — acceptable but prefer SHAKE3-512 |
| ACCEPT | BLAKE3 | OK for speed-critical paths |
| PREFER | SHAKE3-512 | Ideal — FIPS 202, post-quantum |

Detection patterns (multi-language):
- Rust: `md5::`, `sha1::`, `sha2::Sha256`, `ring::digest::SHA256`, `openssl::hash::MessageDigest::sha1()`
- JavaScript: `crypto.createHash('md5')`, `crypto.createHash('sha1')`, `crypto.createHash('sha256')`
- Config: `algorithm: sha1`, `hash_function: md5`
- Git hooks: `.gitattributes` using SHA-1 references

### 1.2 Symmetric Encryption Analyzer (`src/analyzers/symmetric.rs`)

| Status | Algorithm | Action |
|--------|-----------|--------|
| REJECT | DES, 3DES | Error — broken |
| REJECT | RC4 | Error — broken |
| REJECT | AES-ECB | Error — no integrity, patterns visible |
| WARN | AES-CBC | Warning — prefer authenticated encryption |
| WARN | AES-GCM (128-bit) | Warning — prefer 256-bit for quantum margin |
| ACCEPT | AES-GCM (256-bit) | OK |
| PREFER | XChaCha20-Poly1305 | Ideal — larger nonce space, 256-bit |

### 1.3 Key Exchange Analyzer (`src/analyzers/key_exchange.rs`)

| Status | Algorithm | Action |
|--------|-----------|--------|
| REJECT | RSA-1024 | Error — factorable |
| REJECT | DH-1024 | Error — logjam attack |
| WARN | RSA-2048 | Warning — not PQ-safe |
| WARN | ECDH (P-256) | Warning — not PQ-safe |
| ACCEPT | X25519 | OK for classical, not PQ |
| ACCEPT | X448 | OK for classical, not PQ |
| PREFER | Kyber-1024 + SHAKE256-KDF | Ideal — ML-KEM-1024, FIPS 203 |

### 1.4 Signature Analyzer (`src/analyzers/signatures.rs`)

| Status | Algorithm | Action |
|--------|-----------|--------|
| REJECT | RSA-SHA1 | Error — SHA1 broken |
| REJECT | DSA | Error — deprecated |
| WARN | RSA-2048 | Warning — not PQ-safe |
| WARN | Ed25519 | Warning — classical only, prefer Ed448 |
| ACCEPT | Ed448 | OK for classical |
| PREFER | Dilithium5-AES + Ed448 hybrid | Ideal — ML-DSA-87, FIPS 204 |
| FALLBACK | SPHINCS+ | Conservative PQ backup |

### 1.5 Password Hashing Analyzer (`src/analyzers/password.rs`)

| Status | Algorithm | Action |
|--------|-----------|--------|
| REJECT | MD5 (plaintext) | Error — catastrophic |
| REJECT | SHA-1/SHA-256 (unsalted) | Error — rainbow tables |
| WARN | bcrypt | Warning — limited to 72 bytes, 4GB GPU cracking |
| WARN | scrypt | Warning — acceptable but Argon2id preferred |
| ACCEPT | Argon2id (default params) | OK |
| PREFER | Argon2id (512 MiB, 8 iter, 4 lanes) | Ideal — max GPU/ASIC resistance |

### 1.6 TLS/Protocol Analyzer (`src/analyzers/protocol.rs`)

| Status | Protocol | Action |
|--------|----------|--------|
| REJECT | SSLv2, SSLv3, TLS 1.0, TLS 1.1 | Error — deprecated |
| WARN | TLS 1.2 | Warning — prefer TLS 1.3 |
| ACCEPT | TLS 1.3 | OK |
| PREFER | QUIC + HTTP/3 | Ideal |
| REJECT | HTTP (no TLS) | Error — always use HTTPS |
| WARN | IPv4 | Warning — prefer IPv6 |
| PREFER | IPv6 | Ideal |

### 1.7 RNG Analyzer (`src/analyzers/rng.rs`)

| Status | RNG | Action |
|--------|-----|--------|
| REJECT | `rand()`, `Math.random()`, `random.random()` | Error — not cryptographic |
| REJECT | `srand(time(NULL))` | Error — predictable seed |
| WARN | `OsRng` alone | Note — OK but prefer DRBG for reproducibility |
| ACCEPT | `/dev/urandom`, `getrandom()` | OK for seeding |
| PREFER | ChaCha20-DRBG (512-bit seed) | Ideal — SP 800-90Ar1 |

---

## Task 2: Configuration & Dependency Scanning

### 2.1 Dependency crypto audit (`src/analyzers/deps.rs`)
- Parse `Cargo.toml`: check crypto crate versions
  - Flag `ring` versions with known CVEs
  - Flag `openssl` < 3.0 (pre-PQ)
  - Recommend `rustls` over `openssl` where possible
  - Check for `rust-crypto` (unmaintained, reject)
- Parse `package.json`: check crypto package versions
  - Flag `crypto-js` (common, weak defaults)
  - Flag `node-forge` (historical vulnerabilities)
  - Recommend `@noble/ciphers`, `@noble/hashes` (audited)

### 2.2 Configuration file scanning (`src/analyzers/config.rs`)
- Scan `*.toml`, `*.yaml`, `*.yml`, `*.json`, `*.env` for:
  - Hardcoded keys/passwords (entropy analysis)
  - Weak algorithm specifications
  - Insecure protocol settings
  - Self-signed certificate acceptance (`verify_ssl: false`)

---

## Task 3: Post-Quantum Readiness Assessment

### 3.1 PQ readiness scorer (`src/pq_readiness.rs`)

Score each repository's post-quantum preparedness:
- **0-20**: Uses broken crypto (MD5, SHA1, DES) — CRITICAL
- **20-40**: Uses classical-only crypto (RSA, ECDH, Ed25519) — HIGH
- **40-60**: Uses some PQ-ready algorithms but not consistently — MEDIUM
- **60-80**: Hybrid classical+PQ in most places — GOOD
- **80-100**: Full PQ readiness with FIPS compliance — EXCELLENT

### 3.2 Migration roadmap generator
- For each finding, suggest the migration path:
  - `SHA-256` → `SHAKE3-512` (FIPS 202)
  - `Ed25519` → `Ed448 + Dilithium5 hybrid`
  - `AES-GCM-128` → `XChaCha20-Poly1305 (256-bit)`
  - `ECDH P-256` → `Kyber-1024 + X448 hybrid`
- Generate a prioritized migration plan as a Finding

---

## Task 4: CLI Interface

### 4.1 Subcommands
```
cipherbot scan <dir>            # Full crypto hygiene scan
cipherbot analyze <file>        # Single file analysis
cipherbot report <dir>          # Generate SARIF report
cipherbot fleet <dir>           # Run as fleet member
cipherbot pq-readiness <dir>    # Post-quantum readiness assessment
cipherbot audit-deps <dir>      # Dependency crypto audit only
```

### 4.2 Bot modes
- **Advisor**: Report findings, suggest migrations, don't block
- **Consultant**: Only analyze when @cipherbot mentioned
- **Regulator**: Block PRs with REJECT-level findings
- **Policy**: Enforce organization-wide crypto policy from `.bot_directives/cipherbot.scm`

---

## Task 5: Fleet Integration

### 5.1 BotId
- Register as `BotId::Cipherbot` in gitbot-shared-context

### 5.2 Finding categories
- `"crypto/deprecated"` — deprecated algorithms
- `"crypto/weak"` — weak but not broken
- `"crypto/pq-vulnerable"` — classical-only (not PQ-safe)
- `"crypto/config"` — configuration issues
- `"crypto/dependency"` — dependency vulnerabilities
- `"crypto/protocol"` — protocol-level issues

### 5.3 Interaction with other bots
- **sustainabot**: Crypto operations have energy cost — cipherbot can annotate which crypto choices are more energy-efficient (BLAKE3 vs SHAKE3-512 for non-critical hashing)
- **echidnabot**: Cipherbot can verify that proof-carrying code uses appropriate cryptographic primitives for proof integrity
- **panic-attack**: Cipherbot findings complement panic-attack security scanning — crypto hygiene is a different axis from vulnerability detection

---

## Task 6: Policy Engine

### 6.1 Bot directive support
Read `.bot_directives/cipherbot.scm`:
```scheme
(bot-directive
  (name . "cipherbot")
  (allow . #t)
  (scope . ("src" "lib"))
  (mode . "regulator")
  (policy
    (min-hash . "shake3-512")
    (min-symmetric . "xchacha20-poly1305")
    (require-pq . #t)
    (max-key-age-days . 90)
    (allowed-exceptions . ("legacy-compat-module"))))
```

### 6.2 Eclexia policy integration
- Write Eclexia policies for crypto requirements
- `policies/crypto-hash-policy.ecl` — hash algorithm enforcement
- `policies/crypto-pq-policy.ecl` — post-quantum compliance
- `policies/crypto-protocol-policy.ecl` — protocol requirements

---

## Task 7: DNS & Infrastructure Security Analyzer

### 7.1 DNS Zone File Analyzer (`src/analyzers/dns.rs`)
Scan DNS zone files, Cloudflare configs, and infrastructure definitions:

**SPF/DKIM/DMARC checks:**
- Check: SPF record exists and is not overly permissive (`~all` or `-all`, never `+all`)
- Check: DMARC policy is `reject` (not `none` or `quarantine`)
- Check: DKIM records present

**CAA record checks:**
- Check: CAA records restrict certificate issuance to approved CAs
- Check: iodef reporting email configured

**TLSA/DANE checks:**
- Check: TLSA records use SHA-256 minimum (WARN if SHA-1)
- Suggest: migrate to SHAKE3-512 where supported

**MTA-STS checks:**
- Check: MTA-STS policy exists and mode is `enforce` (not `testing`)
- Check: TLS-RPT reporting configured

**SSHFP checks:**
- Check: SSHFP records use SHA-256 hash type (type 2), not just SHA-1 (type 1)
- Warn: SHA-1 SSHFP records present (deprecated)

**Protocol checks:**
- Check: HTTPS enforcement headers/records present
- Check: IPv6 AAAA records present (prefer IPv6)
- Warn: IPv4-only configurations

**Zero Trust checks:**
- Check: internal subdomains use tunnel (cfargotunnel) not direct IPs
- Check: no internal service IPs exposed in public DNS

### 7.2 Infrastructure-as-Code Analyzer (`src/analyzers/infra.rs`)
Scan Terraform, Ansible, Docker/Podman configs:
- Check: TLS minimum version ≥ 1.3
- Check: no hardcoded credentials
- Check: container images use specific SHA digests (not `:latest`)
- Check: network policies restrict egress
- Check: secrets managed through vault/sealed-secrets (not env vars)

---

## Task 8: Ecosystem Integration

### 8.1 Hypatia Integration
- Cipherbot findings feed into Hypatia's neurosymbolic learning loop
- Pattern: recurring crypto issues across repos → Hypatia proposes organization-wide policy
- Cipherbot consumes Hypatia-generated policies for enforcement

### 8.2 a2ml (AI Manifest) Integration
- Read `0-AI-MANIFEST.a2ml` or `AI.a2ml` for repo-specific crypto requirements
- Respect manifest invariants (e.g., "this repo requires FIPS compliance")
- Report findings relative to manifest-declared requirements

### 8.3 k9-svc Integration
- Cipherbot can act as a k9 service contract validator
- Verify that service-to-service communication uses approved crypto
- Validate TLS certificates in k9 service mesh configurations

### 8.4 stateful-artefacts-for-git Integration
- Verify that stateful artifacts (SCM files, state files) use appropriate hashing
- Ensure artifact integrity checking uses SHAKE3-512 or BLAKE3
- Validate signed commits use approved signature algorithms

### 8.5 gitbot-fleet Orchestration
- Publish findings to shared context
- Consume findings from panic-attack (via sustainabot) for correlated security analysis
- Coordinate with echidnabot for proof-carrying code crypto validation
- Feed robot-repo-automaton with auto-fixable crypto migrations

---

## Task 9: Tests

### 7.1 Per-analyzer tests
Each of the 7+ analyzers needs:
- Test with code using approved algorithms → no findings
- Test with code using deprecated algorithms → correct findings
- Test with edge cases (algorithm names in comments vs actual usage)

### 7.2 PQ readiness tests
- Test: repo with all PQ crypto → score 80-100
- Test: repo with all classical crypto → score 20-40
- Test: repo with mixed → appropriate score

### 7.3 Integration test
- Test repo with known crypto patterns
- Verify correct findings for each analyzer
- Verify SARIF output validity
- Verify fleet findings serialize correctly

### Verification
- `cargo test` — minimum 35 tests
- `cargo check` — zero errors
- `cipherbot scan tests/fixtures/` — expected findings
