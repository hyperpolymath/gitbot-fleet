#!/usr/bin/env python3
# SPDX-License-Identifier: MPL-2.0
import copy
import pathlib
import subprocess
import sys
import tempfile
import unittest
import yaml

script = pathlib.Path(__file__).resolve().parents[1] / 'scripts/audit-lock-regeneration.py'
a, b, c = 'a' * 40, 'b' * 40, 'c' * 40
base = {'workflows': {'.github/workflows/test.yml': ['org/reusable@v1']}, 'dependencies': {
    'org/reusable@v1': {'ref': 'v1', 'commit': 'sha1-' + a, 'uses': ['org/composite@' + b]},
    'org/composite@' + b: {'ref': 'v2', 'commit': 'sha1-' + b, 'uses': ['org/child@v3']},
    'org/child@v3': {'ref': 'v3', 'commit': 'sha1-' + c}}}
with tempfile.TemporaryDirectory() as d:
    old, new = pathlib.Path(d) / 'old.yml', pathlib.Path(d) / 'new.yml'
    old.write_text(yaml.safe_dump(base))
    def check(label, doc, expected):
        new.write_text(yaml.safe_dump(doc))
        result = subprocess.run([sys.executable, script, old, new], capture_output=True, text=True)
        assert (result.returncode == 0) == expected, (label, result.stdout, result.stderr)
        print('ok:', label)
    check('unchanged closure', copy.deepcopy(base), True)
    doc = copy.deepcopy(base); doc['dependencies']['org/reusable@v1'].pop('uses'); del doc['dependencies']['org/composite@' + b]; del doc['dependencies']['org/child@v3']
    check('dropped reusable closure without dangling edge', doc, False)
    doc = copy.deepcopy(base); doc['dependencies']['org/composite@' + b].pop('uses'); del doc['dependencies']['org/child@v3']
    check('dropped composite closure without dangling edge', doc, False)
    doc = copy.deepcopy(base); del doc['dependencies']['org/child@v3']; check('dangling edge', doc, False)
    doc = copy.deepcopy(base); doc['dependencies']['org/composite@' + b]['ref'] = 'sha1-' + b; check('lost reviewed tag', doc, False)
    doc = copy.deepcopy(base); doc['dependencies']['org/child@v3']['commit'] = c; check('malformed commit', doc, False)
    doc = copy.deepcopy(base); doc['dependencies']['org/child@v3']['ref'] = 'v4'; check('wrong tag', doc, False)
    doc = copy.deepcopy(base); doc['dependencies']['org/composite@' + b]['commit'] = 'sha1-' + c; check('SHA mismatch', doc, False)


class RegenerationAuditTests(unittest.TestCase):
    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory()
        self.addCleanup(self.tmp.cleanup)
        self.old = pathlib.Path(self.tmp.name) / 'old.yml'
        self.new = pathlib.Path(self.tmp.name) / 'new.yml'

    def run_audit(self, reviewed, proposed):
        for path, doc in ((self.old, reviewed), (self.new, proposed)):
            path.write_text(doc if isinstance(doc, str) else yaml.safe_dump(doc), encoding='utf-8')
        return subprocess.run([sys.executable, script, self.old, self.new], capture_output=True, text=True)

    def test_changed_reviewed_tag_resolution_is_rejected(self):
        doc = copy.deepcopy(base)
        doc['dependencies']['org/child@v3']['commit'] = 'sha1-' + a
        result = self.run_audit(base, doc)
        self.assertEqual(result.returncode, 1, result.stderr)
        self.assertIn('changed reviewed ref/commit org/child@v3', result.stderr)

    def test_changed_reviewed_sha_record_ref_is_rejected(self):
        doc = copy.deepcopy(base)
        doc['dependencies']['org/composite@' + b]['ref'] = 'v4'
        result = self.run_audit(base, doc)
        self.assertEqual(result.returncode, 1, result.stderr)
        self.assertIn('changed reviewed ref/commit org/composite@' + b, result.stderr)

    def test_added_edges_and_new_roots_are_allowed_but_require_review(self):
        doc = copy.deepcopy(base)
        doc['dependencies']['org/child@v3']['uses'] = ['new/action@v1']
        doc['dependencies']['new/action@v1'] = {'ref': 'v1', 'commit': 'sha1-' + a}
        doc['workflows']['new.yml'] = ['new/action@v1']
        result = self.run_audit(base, doc)
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertIn('Preserved reviewed reachable closure (3 records)', result.stdout)
        self.assertIn('new sources require independent review', result.stdout)

    def test_removed_old_root_does_not_force_unused_closure(self):
        proposed = {'workflows': {}, 'dependencies': {}}
        result = self.run_audit(base, proposed)
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertIn('closure (0 records)', result.stdout)

    def test_cycles_and_case_insensitive_repository_names(self):
        reviewed = copy.deepcopy(base)
        reviewed['dependencies']['org/child@v3']['uses'] = ['ORG/Reusable@v1']
        proposed = copy.deepcopy(reviewed)
        proposed['workflows']['.github/workflows/test.yml'] = ['ORG/REUSABLE@v1']
        result = self.run_audit(reviewed, proposed)
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertIn('closure (3 records)', result.stdout)

    def test_dropped_edge_is_identified_even_when_record_remains(self):
        doc = copy.deepcopy(base)
        doc['dependencies']['org/reusable@v1']['uses'] = []
        result = self.run_audit(base, doc)
        self.assertEqual(result.returncode, 1, result.stderr)
        self.assertIn('dropped reviewed edges from org/reusable@v1', result.stderr)
        self.assertIn('org/composite@' + b, result.stderr)

    def test_invalid_documents_fail_on_either_input(self):
        invalid = [
            ('workflows: {}\nworkflows: {}\ndependencies: {}\n', 'duplicate YAML key'),
            ({'workflows': [], 'dependencies': {}}, 'expected workflows and dependencies mappings'),
            ({'workflows': {'test.yml': ['org/missing@v1']}, 'dependencies': {}}, 'missing dependency'),
            ({'workflows': {'test.yml': 'org/action@v1'}, 'dependencies': {}}, 'invalid workflow'),
            ({'workflows': {}, 'dependencies': {'org/action@v1': []}}, 'invalid record'),
            ({'workflows': {}, 'dependencies': {'org/action@v1': {'ref': 'v1', 'commit': 'sha1-' + a, 'uses': 'org/child@v1'}}}, 'invalid uses'),
            ({'workflows': {}, 'dependencies': {'org/action/sub@v1': {}}}, 'invalid dependency reference'),
            ({'workflows': {}, 'dependencies': {'org/action@v1': {'ref': 'v1', 'commit': 'sha1-' + a}, 'ORG/ACTION@v1': {'ref': 'v1', 'commit': 'sha1-' + a}}}, 'duplicate or invalid record'),
        ]
        for doc, message in invalid:
            for old_is_invalid in (True, False):
                with self.subTest(message=message, old_is_invalid=old_is_invalid):
                    result = self.run_audit(doc if old_is_invalid else base, base if old_is_invalid else doc)
                    self.assertEqual(result.returncode, 1, result.stderr)
                    self.assertIn(message, result.stderr)

    def test_cli_requires_two_paths_and_handles_missing_file(self):
        result = subprocess.run([sys.executable, script], capture_output=True, text=True)
        self.assertEqual(result.returncode, 1)
        self.assertIn('usage:', result.stderr)
        self.old.write_text(yaml.safe_dump(base), encoding='utf-8')
        result = subprocess.run([sys.executable, script, self.old, self.new], capture_output=True, text=True)
        self.assertEqual(result.returncode, 1)
        self.assertIn('lock regeneration audit:', result.stderr)


if __name__ == '__main__':
    unittest.main()
