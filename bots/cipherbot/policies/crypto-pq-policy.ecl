; SPDX-License-Identifier: PMPL-1.0-or-later
; Eclexia Policy: Post-Quantum Compliance
;
; Enforces post-quantum readiness across all cryptographic operations.

(eclexia-policy
  (name . "crypto-pq-policy")
  (version . "1.0.0")
  (description . "Post-quantum cryptographic compliance enforcement")

  (rules
    (rule
      (id . "PQ-001")
      (severity . "warning")
      (description . "Classical-only key exchange — migrate to Kyber-1024 hybrid")
      (algorithms . ("RSA" "ECDH" "DH" "X25519"))
      (action . "warn")
      (migration . "Kyber-1024 + SHAKE256-KDF (ML-KEM-1024, FIPS 203)"))

    (rule
      (id . "PQ-002")
      (severity . "warning")
      (description . "Classical-only signatures — migrate to Dilithium5 hybrid")
      (algorithms . ("Ed25519" "RSA-PSS" "ECDSA"))
      (action . "warn")
      (migration . "Ed448 + Dilithium5-AES hybrid (ML-DSA-87, FIPS 204)"))

    (rule
      (id . "PQ-003")
      (severity . "info")
      (description . "Post-quantum key encapsulation in use")
      (algorithms . ("Kyber-1024" "ML-KEM-1024"))
      (action . "accept"))

    (rule
      (id . "PQ-004")
      (severity . "info")
      (description . "Post-quantum signatures in use")
      (algorithms . ("Dilithium5" "ML-DSA-87" "SPHINCS+"))
      (action . "accept"))))
