#!/usr/bin/env python3
# SPDX-License-Identifier: MPL-2.0
import pathlib
import subprocess
import sys
import tempfile
import unittest
import yaml

SCRIPT = pathlib.Path(__file__).resolve().parents[1] / 'scripts/audit-workflow-lock-map.py'


class WorkflowMapTests(unittest.TestCase):
    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory()
        self.addCleanup(self.tmp.cleanup)
        self.root = pathlib.Path(self.tmp.name)
        self.dir = self.root / '.github/workflows'
        self.dir.mkdir(parents=True)
        self.lock = {'workflows': {'.github/workflows/main.yml': ['acme/action@v1'],
                                   '.github/workflows/empty.yml': []},
                     'dependencies': {'acme/action@v1': {'ref': 'v1', 'commit': 'sha1-' + 'a' * 40}}}
        self.write('main.yml', {'jobs': {'build': {'steps': [{'uses': 'Acme/action/subpath@v1'}]}}})
        self.write('empty.yml', {'jobs': {'build': {'steps': [{'run': 'echo hello'}]}}})

    def write(self, name, data):
        (self.dir / name).write_text(yaml.safe_dump(data), encoding='utf-8')

    def run_audit(self, *args):
        (self.dir / 'actions.lock').write_text(yaml.safe_dump(self.lock), encoding='utf-8')
        result = subprocess.run([sys.executable, SCRIPT, self.root, *args], text=True, capture_output=True)
        return result.returncode, result.stdout + result.stderr

    def test_mapped_no_uses_is_not_missing_or_unmanaged(self):
        code, output = self.run_audit()
        self.assertEqual(code, 0, output)
        self.assertIn('NO-USES-MAPPED .github/workflows/empty.yml', output)
        self.assertNotIn('INTENTIONALLY-UNMANAGED', output)
        self.assertIn('remote source closure and provenance NOT VERIFIED', output)

    def test_missing_workflow_is_not_silently_exempted(self):
        del self.lock['workflows']['.github/workflows/empty.yml']
        code, output = self.run_audit()
        self.assertEqual(code, 1, output)
        self.assertIn('MISSING-WORKFLOW .github/workflows/empty.yml', output)

    def test_explicit_unmanaged_is_not_a_closure_pass(self):
        del self.lock['workflows']['.github/workflows/empty.yml']
        code, output = self.run_audit('--unmanaged', '.github/workflows/empty.yml')
        self.assertEqual(code, 2, output)
        self.assertIn('INTENTIONALLY-UNMANAGED', output)
        self.assertIn('closure not verified', output)

    def test_invalid_unmanaged_exception_fails(self):
        code, output = self.run_audit('--unmanaged', '.github/workflows/typo.yml')
        self.assertEqual(code, 1, output)
        self.assertIn('UNMANAGED-UNKNOWN', output)

    def test_missing_direct_ref_even_with_workflow_key(self):
        self.lock['workflows']['.github/workflows/main.yml'] = []
        code, output = self.run_audit()
        self.assertEqual(code, 1, output)
        self.assertIn('MISSING-DIRECT-EDGE', output)

    def test_reusable_missing_edge_is_failure(self):
        self.write('main.yml', {'jobs': {'scan': {'uses': 'acme/suite/.github/workflows/scan.yml@abc'}}})
        self.lock['workflows']['.github/workflows/main.yml'] = []
        code, output = self.run_audit()
        self.assertEqual(code, 1, output)
        self.assertIn('MISSING-DIRECT-EDGE', output)
        self.assertIn('REMOTE-REUSABLE-SOURCE-REVIEW', output)

    def test_missing_nested_record(self):
        self.lock['dependencies']['acme/action@v1']['uses'] = ['acme/child@v1']
        code, output = self.run_audit()
        self.assertEqual(code, 1, output)
        self.assertIn('MISSING-TRANSITIVE-RECORD', output)

    def test_local_reusable_is_checked_at_its_own_path(self):
        self.write('main.yml', {'jobs': {'call': {'uses': './.github/workflows/callee.yml'}}})
        self.lock['workflows']['.github/workflows/main.yml'] = []
        self.write('callee.yml', {'jobs': {'run': {'steps': [{'uses': 'acme/action@v1'}]}}})
        self.lock['workflows']['.github/workflows/callee.yml'] = []
        code, output = self.run_audit()
        self.assertEqual(code, 1, output)
        self.assertIn('LOCAL-REUSABLE', output)
        self.assertIn('MISSING-DIRECT-EDGE .github/workflows/callee.yml', output)

    def test_malformed_and_duplicate_yaml_fail_closed(self):
        self.write('main.yml', {'jobs': {'run': {'steps': [{'uses': 'no-at-sign'}]}}})
        code, output = self.run_audit()
        self.assertEqual(code, 2, output)
        self.assertIn('NOT-RUN', output)
        (self.dir / 'main.yml').write_text('jobs: {}\njobs: {}\n', encoding='utf-8')
        code, output = self.run_audit()
        self.assertEqual(code, 2, output)
        self.assertIn('duplicate YAML key', output)

    def test_extra_edge_is_review_not_proof_of_failure(self):
        self.lock['workflows']['.github/workflows/empty.yml'] = ['acme/action@v1']
        code, output = self.run_audit()
        self.assertEqual(code, 0, output)
        self.assertIn('EXTRA-OR-TRANSITIVE-EDGE', output)


if __name__ == '__main__':
    unittest.main()
