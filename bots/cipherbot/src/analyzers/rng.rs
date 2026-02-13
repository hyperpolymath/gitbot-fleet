// SPDX-License-Identifier: PMPL-1.0-or-later
//! RNG Analyzer — detects usage of non-cryptographic random number generators.
//!
//! | Status  | RNG                          | Action                               |
//! |---------|------------------------------|--------------------------------------|
//! | REJECT  | rand(), Math.random()        | Error — not cryptographic            |
//! | REJECT  | srand(time(NULL))            | Error — predictable seed             |
//! | WARN    | OsRng alone                  | Note — OK but prefer DRBG            |
//! | ACCEPT  | /dev/urandom, getrandom()    | OK for seeding                       |
//! | PREFER  | ChaCha20-DRBG (512-bit seed) | Ideal — SP 800-90Ar1                 |

use crate::analyzers::{Analyzer, CryptoStatus, CryptoUsage};
use regex::Regex;
use std::path::Path;
use std::sync::LazyLock;

struct RngPattern {
    regex: Regex,
    algorithm: &'static str,
    status: CryptoStatus,
    message: &'static str,
    suggestion: Option<&'static str>,
}

static RNG_PATTERNS: LazyLock<Vec<RngPattern>> = LazyLock::new(|| {
    vec![
        // Math.random() — REJECT
        RngPattern {
            regex: Regex::new(r#"(?i)\bMath\.random\s*\(\s*\)"#).unwrap(),
            algorithm: "Math.random",
            status: CryptoStatus::Reject,
            message: "Math.random() is not cryptographically secure — predictable output.",
            suggestion: Some("Use crypto.getRandomValues() or ChaCha20-DRBG"),
        },
        // C rand() — REJECT
        RngPattern {
            regex: Regex::new(r#"\b(?:std::)?rand\s*\(\s*\)"#).unwrap(),
            algorithm: "C-rand",
            status: CryptoStatus::Reject,
            message: "C rand() is not cryptographically secure — use a CSPRNG instead.",
            suggestion: Some("Use ChaCha20-DRBG with 512-bit seed (SP 800-90Ar1)"),
        },
        // srand(time(NULL)) — REJECT
        RngPattern {
            regex: Regex::new(r#"\bsrand\s*\(\s*time\s*\(\s*NULL\s*\)\s*\)"#).unwrap(),
            algorithm: "srand-time",
            status: CryptoStatus::Reject,
            message: "srand(time(NULL)) produces predictable seeds — trivially guessable.",
            suggestion: Some("Use getrandom() or /dev/urandom for seeding, then ChaCha20-DRBG"),
        },
        // Python random.random() — REJECT
        RngPattern {
            regex: Regex::new(r#"(?i)\brandom\.(?:random|randint|choice|shuffle)\s*\("#).unwrap(),
            algorithm: "Python-random",
            status: CryptoStatus::Reject,
            message: "Python random module is not cryptographically secure — uses Mersenne Twister.",
            suggestion: Some("Use secrets module or ChaCha20-DRBG"),
        },
        // OsRng — WARN (note, acceptable)
        RngPattern {
            regex: Regex::new(r#"(?i)\bOsRng\b"#).unwrap(),
            algorithm: "OsRng",
            status: CryptoStatus::Warn,
            message: "OsRng is cryptographically secure but consider a DRBG for reproducibility in testing.",
            suggestion: Some("Consider ChaCha20-DRBG seeded from OsRng for deterministic testing"),
        },
        // /dev/urandom, getrandom — ACCEPT
        RngPattern {
            regex: Regex::new(r#"(?:/dev/urandom|getrandom\s*\()"#).unwrap(),
            algorithm: "urandom",
            status: CryptoStatus::Accept,
            message: "/dev/urandom or getrandom() — acceptable for CSPRNG seeding.",
            suggestion: None,
        },
        // ChaCha20-DRBG — PREFER
        RngPattern {
            regex: Regex::new(r#"(?i)\b(?:chacha20[_-]?drbg|ChaCha20Rng|chacha_rng)\b"#).unwrap(),
            algorithm: "ChaCha20-DRBG",
            status: CryptoStatus::Prefer,
            message: "ChaCha20-DRBG (SP 800-90Ar1) — ideal CSPRNG with deterministic output from seed.",
            suggestion: None,
        },
        // ThreadRng (uses ChaCha internally but worth noting) — ACCEPT
        RngPattern {
            regex: Regex::new(r#"(?i)\b(?:thread_rng|ThreadRng)\b"#).unwrap(),
            algorithm: "ThreadRng",
            status: CryptoStatus::Accept,
            message: "thread_rng() uses ChaCha20 internally — acceptable for most uses.",
            suggestion: None,
        },
    ]
});

/// Analyzer for random number generator usage.
pub struct RngAnalyzer;

impl Analyzer for RngAnalyzer {
    fn name(&self) -> &str {
        "RNG Analyzer"
    }

    fn category(&self) -> &str {
        "crypto/weak"
    }

    fn analyze_content(&self, path: &Path, content: &str) -> Vec<CryptoUsage> {
        let mut usages = Vec::new();

        for (line_num, line) in content.lines().enumerate() {
            let trimmed = line.trim();
            if trimmed.starts_with("//") || trimmed.starts_with('#') || trimmed.starts_with("/*") {
                continue;
            }

            for pattern in RNG_PATTERNS.iter() {
                if pattern.regex.is_match(line) {
                    usages.push(CryptoUsage {
                        algorithm: pattern.algorithm.to_string(),
                        status: pattern.status,
                        file: path.to_path_buf(),
                        line: line_num + 1,
                        matched_text: line.trim().to_string(),
                        category: self.category().to_string(),
                        message: pattern.message.to_string(),
                        suggestion: pattern.suggestion.map(String::from),
                    });
                }
            }
        }
        usages
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_detect_math_random() {
        let analyzer = RngAnalyzer;
        let content = "let x = Math.random();";
        let usages = analyzer.analyze_content(Path::new("test.js"), content);
        assert!(!usages.is_empty(), "Should detect Math.random()");
        assert_eq!(usages[0].status, CryptoStatus::Reject);
    }

    #[test]
    fn test_detect_srand_time() {
        let analyzer = RngAnalyzer;
        let content = "srand(time(NULL));";
        let usages = analyzer.analyze_content(Path::new("test.c"), content);
        assert!(!usages.is_empty(), "Should detect srand(time(NULL))");
        assert_eq!(usages[0].status, CryptoStatus::Reject);
    }

    #[test]
    fn test_detect_python_random() {
        let analyzer = RngAnalyzer;
        let content = "x = random.randint(0, 100)";
        let usages = analyzer.analyze_content(Path::new("test.py"), content);
        assert!(!usages.is_empty(), "Should detect Python random");
        assert_eq!(usages[0].status, CryptoStatus::Reject);
    }

    #[test]
    fn test_detect_osrng_warn() {
        let analyzer = RngAnalyzer;
        let content = "let mut rng = OsRng;";
        let usages = analyzer.analyze_content(Path::new("test.rs"), content);
        assert!(!usages.is_empty(), "Should detect OsRng");
        assert_eq!(usages[0].status, CryptoStatus::Warn);
    }

    #[test]
    fn test_detect_chacha20_drbg_prefer() {
        let analyzer = RngAnalyzer;
        let content = "let rng = ChaCha20Rng::from_seed(seed);";
        let usages = analyzer.analyze_content(Path::new("test.rs"), content);
        assert!(!usages.is_empty(), "Should detect ChaCha20-DRBG");
        assert_eq!(usages[0].status, CryptoStatus::Prefer);
    }
}
