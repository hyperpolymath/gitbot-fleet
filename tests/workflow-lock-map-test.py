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

    def test_sha_reusable_empty_map_requires_source_review(self):
        self.write('main.yml', {'jobs': {'scan': {'uses': 'acme/suite/.github/workflows/scan.yml@' + 'a' * 40}}})
        self.lock['workflows']['.github/workflows/main.yml'] = []
        code, output = self.run_audit()
        self.assertEqual(code, 0, output)
        self.assertIn('REMOTE-REUSABLE-SOURCE-REVIEW', output)
        self.assertIn('UNSUPPORTED-LOCK-SCOPE', output)
        self.assertNotIn('MISSING-DIRECT-EDGE', output)
        self.assertNotIn('NO-USES-MAPPED .github/workflows/main.yml', output)

    def test_remote_reusable_floating_and_short_pins_fail(self):
        for ref in ('main', 'v1', 'abc1234', 'a' * 39, 'a' * 41, 'g' * 40):
            with self.subTest(ref=ref):
                self.write('main.yml', {'jobs': {'scan': {'uses': 'acme/suite/.github/workflows/scan.yml@' + ref}}})
                self.lock['workflows']['.github/workflows/main.yml'] = []
                code, output = self.run_audit()
                self.assertEqual(code, 1, output)
                self.assertIn('REMOTE-REUSABLE-NOT-SHA', output)

    def test_reusable_does_not_exempt_missing_ordinary_action_edge(self):
        self.write('main.yml', {'jobs': {
            'scan': {'uses': 'acme/suite/.github/workflows/scan.yml@' + 'a' * 40},
            'build': {'steps': [{'uses': 'acme/action@v1'}]},
        }})
        self.lock['workflows']['.github/workflows/main.yml'] = []
        code, output = self.run_audit()
        self.assertEqual(code, 1, output)
        self.assertIn('MISSING-DIRECT-EDGE .github/workflows/main.yml acme/action@v1', output)
        self.assertIn('UNSUPPORTED-LOCK-SCOPE', output)

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

    def test_mapped_ref_without_record_and_stale_workflow(self):
        self.lock['workflows']['.github/workflows/main.yml'].append('acme/missing@v2')
        self.lock['workflows']['.github/workflows/deleted.yml'] = []
        code, output = self.run_audit()
        self.assertEqual(code, 1, output)
        self.assertIn('MISSING-RECORD .github/workflows/main.yml acme/missing@v2', output)
        self.assertIn('STALE-WORKFLOW .github/workflows/deleted.yml', output)

    def test_mapped_workflow_cannot_also_be_unmanaged(self):
        code, output = self.run_audit('--unmanaged', '.github/workflows/main.yml')
        self.assertEqual(code, 1, output)
        self.assertIn('UNMANAGED-BUT-MAPPED .github/workflows/main.yml', output)

    def test_invalid_local_workflow_paths(self):
        for path in ('./.github/workflows/missing.yml', './outside.yml', './.github/workflows/actions.lock'):
            with self.subTest(path=path):
                self.write('main.yml', {'jobs': {'call': {'uses': path}}})
                self.lock['workflows']['.github/workflows/main.yml'] = []
                code, output = self.run_audit()
                self.assertEqual(code, 1, output)
                self.assertIn('INVALID-LOCAL-WORKFLOW .github/workflows/main.yml ' + path, output)

    def test_local_actions_and_docker_are_reported_for_review(self):
        self.write('main.yml', {'jobs': {'build': {'steps': [
            {'uses': './my-action'}, {'uses': 'docker://example/image:1'}]}}})
        self.lock['workflows']['.github/workflows/main.yml'] = []
        code, output = self.run_audit()
        self.assertEqual(code, 0, output)
        self.assertIn('NOT-LOCKED-SOURCE-REVIEW .github/workflows/main.yml ./my-action', output)
        self.assertIn('NOT-LOCKED-SOURCE-REVIEW .github/workflows/main.yml docker://example/image:1', output)

    def test_normalizes_owner_repo_and_subpath_but_preserves_ref_case(self):
        self.write('main.yml', {'jobs': {'build': {'steps': [{'uses': 'ACME/ACTION/sub@V1'}]}}})
        code, output = self.run_audit()
        self.assertEqual(code, 1, output)
        self.assertIn('MISSING-DIRECT-EDGE .github/workflows/main.yml acme/action@V1', output)
        self.lock['workflows']['.github/workflows/main.yml'] = ['acme/action@V1']
        self.lock['dependencies'] = {'Acme/Action@V1': {}}
        code, output = self.run_audit()
        self.assertEqual(code, 0, output)

    def test_duplicate_normalized_refs_and_bad_records_fail_closed(self):
        cases = [
            ('duplicate dependency', lambda: self.lock['dependencies'].update({'ACME/ACTION@v1': {}})),
            ('duplicate map edge', lambda: self.lock['workflows']['.github/workflows/main.yml'].append('ACME/ACTION/sub@v1')),
            ('invalid record', lambda: self.lock['dependencies'].update({'acme/action@v1': []})),
            ('invalid transitive edge', lambda: self.lock['dependencies']['acme/action@v1'].update({'uses': ['./local']})),
            ('invalid workflow map', lambda: self.lock['workflows'].update({'.github/workflows/main.yml': 'acme/action@v1'})),
        ]
        original = yaml.safe_dump(self.lock)
        for label, mutate in cases:
            with self.subTest(label=label):
                self.lock = yaml.safe_load(original)
                mutate()
                code, output = self.run_audit()
                self.assertEqual(code, 2, output)
                self.assertIn('NOT-RUN:', output)

    def test_invalid_workflow_shapes_fail_closed(self):
        cases = [
            {'jobs': []},
            {'jobs': {'build': None}},
            {'jobs': {'build': {'steps': {}}}},
            {'jobs': {'build': {'steps': [None]}}},
            {'jobs': {'build': {'steps': [{'uses': 'acme/action@bad ref'}]}}},
        ]
        for doc in cases:
            with self.subTest(doc=doc):
                self.write('main.yml', doc)
                code, output = self.run_audit()
                self.assertEqual(code, 2, output)
                self.assertIn('NOT-RUN:', output)

    def test_yaml_extension_is_inventoried(self):
        self.write('extra.yaml', {'jobs': {'run': {'steps': []}}})
        code, output = self.run_audit()
        self.assertEqual(code, 1, output)
        self.assertIn('MISSING-WORKFLOW .github/workflows/extra.yaml', output)


class ScannerCallerTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        root = pathlib.Path(__file__).resolve().parents[1] / '.github/workflows'
        # BaseLoader preserves GitHub's `on` key and the spelling of scalar values.
        cls.workflow = yaml.load((root / 'secret-scanner.yml').read_text(encoding='utf-8'), Loader=yaml.BaseLoader)
        cls.lock = yaml.safe_load((root / 'actions.lock').read_text(encoding='utf-8'))

    def test_scanner_triggers_and_access(self):
        self.assertEqual(self.workflow['on'], {'pull_request': '', 'push': {'branches': ['main']}})
        self.assertEqual(self.workflow['permissions'], {'contents': 'read'})
        self.assertEqual(set(self.workflow['jobs']), {'scan'})
        job = self.workflow['jobs']['scan']
        self.assertEqual(job['permissions'], {'contents': 'read'})
        self.assertNotIn('secrets', job)
        self.assertEqual(job['with'], {'fast-pr': 'false'})

    def test_scanner_source_pin_and_supported_lock_scope(self):
        sha = '4404c3c0474e17753fe7689f3485c02890aac113'
        suite = 'hyperpolymath/cicd-suite@' + sha
        checkout = 'actions/checkout@3d3c42e5aac5ba805825da76410c181273ba90b1'
        self.assertEqual(self.workflow['jobs']['scan']['uses'],
                         'hyperpolymath/cicd-suite/.github/workflows/secret-scanner-reusable.yml@' + sha)
        # gh-actions-lock v0.4.1 does not lock external jobs.<id>.uses.
        # The caller SHA above is checked separately from ordinary action records.
        self.assertEqual(self.lock['workflows']['.github/workflows/secret-scanner.yml'], [])
        self.assertNotIn(suite, self.lock['dependencies'])
        self.assertIn(checkout, self.lock['dependencies'])
        self.assertEqual(self.lock['dependencies'][checkout]['commit'],
                         'sha1-3d3c42e5aac5ba805825da76410c181273ba90b1')


if __name__ == '__main__':
    unittest.main()
