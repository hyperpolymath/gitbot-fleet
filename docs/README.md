<!-- SPDX-License-Identifier: MPL-2.0 -->
<!-- SPDX-FileCopyrightText: 2025-2026 Jonathan D.A. Jewell -->

# Gitbot Fleet Documentation

Authoritative documents and operational guides for the gitbot-fleet.

## Authoritative

| Document | Purpose |
|---|---|
| [`ARCHITECTURE.md`](ARCHITECTURE.md) | Single source of truth for the system architecture. The repo-root `README.adoc` links here. |
| [`BOT-OPERATIONS.md`](BOT-OPERATIONS.md) | Long-form operational guide for running and supervising the fleet. |
| [`PERFORMANCE.md`](PERFORMANCE.md) | Benchmark notes, profiling traces, latency targets. |
| [`BRANCH-PROTECTION-SETUP.md`](BRANCH-PROTECTION-SETUP.md) | How to wire branch-protection rules for fleet-managed repos. |

## Wiki source

[`wiki-source/`](wiki-source/) holds the source Markdown for this repo's
GitHub wiki. Push it to the wiki via:

```bash
git clone git@github.com:hyperpolymath/gitbot-fleet.wiki.git
cp -R docs/wiki-source/* gitbot-fleet.wiki/
cd gitbot-fleet.wiki && git add -A && git commit -m "Sync from gitbot-fleet/docs/wiki-source" && git push
```

The repo is the source of truth; the wiki is a publication target.

## Archive

[`archive/`](archive/) preserves dated session reports and historical
status snapshots. These are no longer authoritative but are kept for
audit / change-context tracing. See [`archive/README.md`](archive/README.md).
