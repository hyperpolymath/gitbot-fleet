#!/usr/bin/env bash
# SPDX-License-Identifier: PMPL-1.0-or-later
#
# fix-license-file.sh — Auto-create LICENSE with PMPL-1.0-or-later text
#
# Category: LicenseCompliance
# Usage: fix-license-file.sh <repo-path> <finding-json>
#
# Idempotent: exits 0 if any license file already exists.
# Does NOT commit — dispatch-runner handles that.

set -euo pipefail

REPO_PATH="${1:?Usage: fix-license-file.sh <repo-path> <finding-json>}"
FINDING_JSON="${2:?Usage: fix-license-file.sh <repo-path> <finding-json>}"

# --- Idempotency check ---
for candidate in LICENSE LICENSE.txt LICENSE.md COPYING; do
  if [[ -f "${REPO_PATH}/${candidate}" ]]; then
    echo "[fix-license-file] ${candidate} already exists — skipping."
    exit 0
  fi
done

# --- Create LICENSE ---
cat > "${REPO_PATH}/LICENSE" <<'LICENSETEXT'
Palimpsest License (PMPL-1.0-or-later)

Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

Permission is hereby granted to any person obtaining a copy of this software
and associated documentation files (the "Software"), to use, copy, modify,
merge, publish, distribute, sublicense, and/or sell copies of the Software,
subject to the following conditions:

1. The above copyright notice and this permission notice shall be included in
   all copies or substantial portions of the Software.

2. Modified versions must be clearly marked as such, and must not be
   misrepresented as being the original software.

3. This license text may not be removed or altered from any distribution.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
LICENSETEXT

echo "[fix-license-file] Created LICENSE with PMPL-1.0-or-later text."
