#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# Execute the changed audit steps offline with synthetic command responses.
# Usage: bash scripts/tests/pr-audit-workflows-test.sh
set -euo pipefail
ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
python3 - "$ROOT" <<'PY'
import os
from pathlib import Path
import subprocess
import sys
import tempfile
import unittest

import yaml

ROOT = Path(sys.argv.pop())


def step(workflow, name):
    jobs = yaml.safe_load((ROOT / '.github/workflows' / workflow).read_text())['jobs']
    matches = [s['run'] for job in jobs.values() for s in job.get('steps', [])
               if s.get('name') == name]
    if len(matches) != 1:
        raise AssertionError(f'expected exactly one step named {name!r}')
    return matches[0]


class AuditWorkflowTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.source = step('e2e.yml', 'Verify reviewed source closure against regeneration')
        cls.advisories = step('rust.yml', 'Audit enabled dependencies (no advisory exclusions)')

    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory(prefix='pr-audit-workflows-')
        self.addCleanup(self.tmp.cleanup)
        self.tree = Path(self.tmp.name)
        (self.tree / 'bin').mkdir()
        (self.tree / 'scripts').mkdir()
        self.env = dict(os.environ, PATH=f"{self.tree / 'bin'}:{os.environ['PATH']}",
                        RUNNER_TEMP=str(self.tree), BASH_ENV='')
        # Keep every subprocess offline even if a future workflow invokes gh.
        self.command('bin/gh', 'exit 99')

    def command(self, name, body):
        path = self.tree / name
        path.write_text('#!/bin/bash\nset -eu\n' + body + '\n')
        path.chmod(0o755)

    def run_step(self, script):
        return subprocess.run(['/bin/bash', '--noprofile', '--norc', '-e', '-c', script],
                              cwd=self.tree, env=self.env, text=True,
                              stdout=subprocess.PIPE, stderr=subprocess.STDOUT, timeout=5)

    def source_result(self, rc, diagnostic):
        (self.tree / 'diagnostic').write_text(diagnostic)
        self.env['TEST_RC'] = str(rc)
        self.command('scripts/audit-pinned-reusable-sources.sh', '''
[ "$#" -eq 4 ]
[ "$1" = --repo-root ]
[ "$2" = "$RUNNER_TEMP/lock-regeneration" ]
[ "$3" = --lock ]
[ "$4" = "$RUNNER_TEMP/lock-regeneration/.github/workflows/actions.lock" ]
cat "$RUNNER_TEMP/diagnostic" >&2
exit "$TEST_RC"''')
        return self.run_step(self.source)

    def test_source_audit_success_has_no_error_annotation(self):
        result = self.source_result(0, 'authenticated closure OK\n')
        self.assertEqual(result.returncode, 0, result.stdout)
        self.assertIn('authenticated closure OK', result.stdout)
        self.assertNotIn('::error::', result.stdout)

    def test_findings_and_missing_evidence_fail_with_original_exit_code(self):
        for rc in (1, 2, 3):
            with self.subTest(rc=rc):
                result = self.source_result(rc, 'bad%receipt\r\n::notice::untrusted\n')
                self.assertEqual(result.returncode, rc, result.stdout)
                annotation = [s for s in result.stdout.splitlines() if s.startswith('::error::')]
                self.assertEqual(len(annotation), 1)
                self.assertEqual(annotation[0], f'::error::Regenerated source audit exited {rc}: '
                                 'bad%25receipt%0D%0A::notice::untrusted')

    def test_source_annotation_is_bounded_but_full_diagnostic_is_retained(self):
        result = self.source_result(2, 'x' * 3500)
        self.assertEqual(result.returncode, 2)
        self.assertEqual((self.tree / 'regenerated-source-audit.txt').read_text(), 'x' * 3500)
        annotation = result.stdout.split('::error::')[1]
        self.assertEqual(annotation.strip(), 'Regenerated source audit exited 2: ' + 'x' * 3000)

    def cargo_result(self, rc, diagnostic, tee_rc=None):
        (self.tree / 'diagnostic').write_text(diagnostic)
        self.env['TEST_RC'] = str(rc)
        self.command('bin/cargo', '''
[ "$*" = 'deny --locked --config deny.toml check advisories --warn unmaintained' ]
cat "$RUNNER_TEMP/diagnostic" >&2
exit "$TEST_RC"''')
        if tee_rc is not None:
            self.env['TEST_TEE_RC'] = str(tee_rc)
            # Consume the entire pipe so cargo does not fail spuriously on SIGPIPE.
            self.command('bin/tee', 'cat > "$1"\ncat "$1"\nexit "$TEST_TEE_RC"')
        return self.run_step(self.advisories)

    def test_advisory_success_preserves_logs_and_has_no_error(self):
        result = self.cargo_result(0, 'advisories ok\n')
        self.assertEqual(result.returncode, 0, result.stdout)
        self.assertEqual((self.tree / 'gsbot-advisories.log').read_text(), 'advisories ok\n')
        self.assertNotIn('::error::', result.stdout)

    def test_advisory_failure_escapes_matching_diagnostics(self):
        result = self.cargo_result(7, 'error: RUSTSEC-test 100%\r\nfailed to audit\n')
        self.assertEqual(result.returncode, 7, result.stdout)
        self.assertIn('::error::GSBot dependency audit failed (exit 7): '
                      'error: RUSTSEC-test 100%25%0D%0Afailed to audit', result.stdout)

    def test_failure_without_matching_diagnostic_has_actionable_fallback(self):
        result = self.cargo_result(9, 'unexpected termination\n')
        self.assertEqual(result.returncode, 9, result.stdout)
        self.assertIn('Audit failed; inspect the full job log.', result.stdout)

    def test_tee_failure_is_not_a_success_and_cargo_failure_takes_precedence(self):
        for cargo_rc, expected in [(0, 5), (7, 7)]:
            with self.subTest(cargo_rc=cargo_rc):
                result = self.cargo_result(cargo_rc, 'error: synthetic failure\n', tee_rc=5)
                self.assertEqual(result.returncode, expected, result.stdout)
                self.assertIn(f'dependency audit failed (exit {expected})', result.stdout)

    def test_source_audit_without_diagnostics_still_fails(self):
        result = self.source_result(17, '')
        self.assertEqual(result.returncode, 17, result.stdout)
        self.assertIn('::error::Regenerated source audit exited 17:', result.stdout)
        self.assertEqual((self.tree / 'regenerated-source-audit.txt').read_text(), '')

    def test_successful_audit_does_not_treat_log_words_as_failure(self):
        result = self.cargo_result(0, 'warning: synthetic diagnostic\nRUSTSEC-example\n')
        self.assertEqual(result.returncode, 0, result.stdout)
        self.assertNotIn('::error::', result.stdout)

    def test_diagnostic_escaping_is_not_applied_twice(self):
        result = self.cargo_result(1, 'error: literal %0A and %25\n')
        self.assertEqual(result.returncode, 1, result.stdout)
        self.assertIn('error: literal %250A and %2525', result.stdout)
        self.assertEqual((self.tree / 'gsbot-advisories.log').read_text(),
                         'error: literal %0A and %25\n')

    def test_advisory_annotation_is_bounded(self):
        result = self.cargo_result(1, 'error: ' + 'x' * 6000)
        self.assertEqual(result.returncode, 1)
        self.assertEqual((self.tree / 'gsbot-advisories.log').stat().st_size, 6007)
        annotation = result.stdout.split('::error::')[1].strip()
        self.assertEqual(annotation, 'GSBot dependency audit failed (exit 1): error: ' + 'x' * 4993)


unittest.main()
PY
