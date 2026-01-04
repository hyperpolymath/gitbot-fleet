<!-- SPDX-License-Identifier: MPL-2.0 -->
<!-- SPDX-FileCopyrightText: 2025 hyperpolymath -->

# Security Policy

## Supported Versions

| Version | Supported          |
| ------- | ------------------ |
| main    | :white_check_mark: |

## Reporting a Vulnerability

To report a security vulnerability, please use GitHub's private vulnerability reporting feature:

1. Go to the Security tab of this repository
2. Click "Report a vulnerability"
3. Provide details about the vulnerability

**Do not** open public issues for security vulnerabilities.

## Security Measures

This project follows the Hyperpolymath security standards:

- SPDX license headers on all files
- SHA-pinned GitHub Actions
- CodeQL analysis enabled
- OpenSSF Scorecard monitoring
- Branch protection enabled

## Cryptographic Standards

This project adheres to `CRYPTO-STANDARD.scm`:

- Password hashing: Argon2id
- General hashing: BLAKE3
- Post-quantum signatures: Dilithium5 (ML-DSA-87)
- Post-quantum key exchange: Kyber-1024 (ML-KEM-1024)
- Classical signatures: Ed448 (Ed25519 acceptable for SSH)
