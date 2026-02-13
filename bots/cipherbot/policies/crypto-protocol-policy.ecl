; SPDX-License-Identifier: PMPL-1.0-or-later
; Eclexia Policy: Protocol Requirements
;
; Enforces minimum protocol standards for all network communications.

(eclexia-policy
  (name . "crypto-protocol-policy")
  (version . "1.0.0")
  (description . "Network protocol compliance enforcement")

  (rules
    (rule
      (id . "PROTO-001")
      (severity . "error")
      (description . "SSL 2.0/3.0 and TLS 1.0/1.1 are deprecated")
      (protocols . ("SSLv2" "SSLv3" "TLS1.0" "TLS1.1"))
      (action . "reject"))

    (rule
      (id . "PROTO-002")
      (severity . "warning")
      (description . "TLS 1.2 — prefer TLS 1.3")
      (protocols . ("TLS1.2"))
      (action . "warn"))

    (rule
      (id . "PROTO-003")
      (severity . "error")
      (description . "Plaintext HTTP prohibited for non-localhost")
      (pattern . "http://")
      (exceptions . ("localhost" "127.0.0.1" "::1"))
      (action . "reject"))

    (rule
      (id . "PROTO-004")
      (severity . "info")
      (description . "QUIC + HTTP/3 is the preferred transport")
      (protocols . ("QUIC" "HTTP/3"))
      (action . "accept"))

    (rule
      (id . "PROTO-005")
      (severity . "warning")
      (description . "IPv4-only — prefer IPv6")
      (action . "warn"))))
