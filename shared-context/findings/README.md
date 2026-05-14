# Findings Directory

This directory receives security findings from repos running Hypatia scans.

## Structure

```
findings/
├── <repo-name>/
│   ├── <scan-timestamp>.json       # Individual scan results
│   └── latest.json -> <most-recent>
└── consolidated/
    └── all-findings-<date>.json    # Aggregated findings
```

## Submission

Findings are submitted via GitHub Actions from individual repos using:
- Secure token authentication
- JSON schema validation
- Deduplication by issue hash

## Processing

Fleet coordinator processes findings via:
1. `fleet-coordinator.sh process-findings`
2. Bot execution based on severity
3. Learning engine observation

## Schema

Each finding must conform to:
```json
{
  "repo": "owner/repo-name",
  "scan_timestamp": "2026-01-29T13:00:00Z",
  "commit": "sha256...",
  "findings": [
    {
      "id": "unique-hash",
      "type": "security|waste|quality",
      "severity": "critical|high|medium|low",
      "message": "Description",
      "location": {"file": "path", "line": 42},
      "auto_fixable": true|false,
      "fix_suggestion": "One-line fix description"
    }
  ]
}
```
