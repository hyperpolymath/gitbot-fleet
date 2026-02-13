; SPDX-License-Identifier: PMPL-1.0-or-later
; Eclexia Policy: Cryptographic Hash Algorithm Enforcement
;
; This policy defines the minimum acceptable hash algorithms
; for all hyperpolymath repositories.

(eclexia-policy
  (name . "crypto-hash-policy")
  (version . "1.0.0")
  (description . "Hash algorithm enforcement for cryptographic hygiene")

  (rules
    (rule
      (id . "HASH-001")
      (severity . "error")
      (description . "MD5 usage is prohibited")
      (pattern . "md5")
      (action . "reject"))

    (rule
      (id . "HASH-002")
      (severity . "error")
      (description . "SHA-1 usage is prohibited")
      (pattern . "sha1")
      (action . "reject"))

    (rule
      (id . "HASH-003")
      (severity . "warning")
      (description . "SHA-256 alone is not PQ-safe — use SHAKE3-512")
      (pattern . "sha256")
      (action . "warn"))

    (rule
      (id . "HASH-004")
      (severity . "info")
      (description . "SHAKE3-512 is the preferred hash function")
      (pattern . "shake3-512")
      (action . "accept"))))
