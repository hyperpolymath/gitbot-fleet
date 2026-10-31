#!/usr/bin/env python3
# SPDX-License-Identifier: MPL-2.0
import copy
import pathlib
import subprocess
import sys
import tempfile
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
