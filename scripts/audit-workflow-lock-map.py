#!/usr/bin/env python3
# SPDX-License-Identifier: MPL-2.0
"""Inventory workflow lock-map coverage, not remote source closure or provenance."""
import argparse
import pathlib
import re
import sys
import yaml


class UniqueLoader(yaml.SafeLoader):
    pass


def unique_mapping(loader, node, deep=False):
    result = {}
    for key, value in node.value:
        k = loader.construct_object(key, deep=deep)
        if k in result:
            raise ValueError(f'duplicate YAML key: {k!r}')
        result[k] = loader.construct_object(value, deep=deep)
    return result


UniqueLoader.add_constructor(yaml.resolver.BaseResolver.DEFAULT_MAPPING_TAG, unique_mapping)


def load(path):
    with path.open(encoding='utf-8') as f:
        return yaml.load(f, Loader=UniqueLoader)


def ref_key(value):
    if not isinstance(value, str) or not value or re.search(r'\s', value):
        raise ValueError(f'invalid uses: {value!r}')
    if value.startswith('./'):
        return None
    if value.startswith('docker://'):
        return None
    m = re.fullmatch(r'([^/@]+)/([^/@]+)(?:/[^@]+)?@([^@]+)', value)
    if not m:
        raise ValueError(f'unsupported or invalid uses: {value!r}')
    return f'{m[1].lower()}/{m[2].lower()}@{m[3]}'


def inspect(root, unmanaged=()):
    root = pathlib.Path(root).resolve()
    wfdir = root / '.github/workflows'
    lock = load(wfdir / 'actions.lock')
    if not isinstance(lock, dict) or not isinstance(lock.get('workflows'), dict) or not isinstance(lock.get('dependencies'), dict):
        raise ValueError('invalid workflows or dependencies mapping')
    workflows, deps = lock['workflows'], lock['dependencies']
    files = {p.relative_to(root).as_posix(): p for p in sorted(wfdir.glob('*')) if p.suffix in ('.yml', '.yaml') and p.is_file()}
    unmanaged = set(unmanaged)
    findings, notes = [], []
    for path in sorted(unmanaged - files.keys()):
        findings.append(f'UNMANAGED-UNKNOWN {path}')
    for path in sorted(workflows.keys() - files.keys()):
        findings.append(f'STALE-WORKFLOW {path}')
    depkeys = set()
    for dep in deps:
        norm = ref_key(dep)
        if norm is None or norm in depkeys:
            raise ValueError(f'invalid or duplicate dependency key: {dep!r}')
        depkeys.add(norm)
    for dep, record in deps.items():
        if not isinstance(record, dict) or not isinstance(record.get('uses', []), list):
            raise ValueError(f'invalid dependency record: {dep!r}')
        for edge in record.get('uses', []):
            normalized = ref_key(edge)
            if normalized is None:
                raise ValueError(f'invalid dependency edge: {edge!r}')
            if normalized not in depkeys:
                findings.append(f'MISSING-TRANSITIVE-RECORD {dep} {normalized}')
    for name, file in files.items():
        doc = load(file)
        if not isinstance(doc, dict) or not isinstance(doc.get('jobs'), dict):
            raise ValueError(f'{name}: expected jobs mapping')
        refs, local_calls, unsupported = set(), [], []
        for job in doc['jobs'].values():
            if not isinstance(job, dict):
                raise ValueError(f'{name}: invalid job')
            entries = []
            if 'uses' in job:
                entries.append((job['uses'], True))
            steps = job.get('steps', [])
            if not isinstance(steps, list):
                raise ValueError(f'{name}: invalid steps')
            for step in steps:
                if not isinstance(step, dict):
                    raise ValueError(f'{name}: invalid step')
                if 'uses' in step:
                    entries.append((step['uses'], False))
            for raw, is_job in entries:
                normalized = ref_key(raw)
                if normalized:
                    refs.add(normalized)
                    if is_job:
                        notes.append(f'REMOTE-REUSABLE-SOURCE-REVIEW {name} {normalized}')
                elif raw.startswith('./') and is_job:
                    local = (root / raw).resolve()
                    if not local.is_relative_to(wfdir) or local.suffix not in ('.yml', '.yaml') or not local.is_file():
                        findings.append(f'INVALID-LOCAL-WORKFLOW {name} {raw}')
                    else:
                        local_calls.append(local.relative_to(root).as_posix())
                else:
                    unsupported.append(raw)
        for raw in unsupported:
            notes.append(f'NOT-LOCKED-SOURCE-REVIEW {name} {raw}')
        if name not in workflows:
            if name in unmanaged:
                notes.append(f'INTENTIONALLY-UNMANAGED {name} (dependency closure not verified)')
            else:
                findings.append(f'MISSING-WORKFLOW {name}')
            continue
        if name in unmanaged:
            findings.append(f'UNMANAGED-BUT-MAPPED {name}')
        mapped = workflows[name]
        if not isinstance(mapped, list):
            raise ValueError(f'{name}: expected lock list')
        normalized_mapped = [ref_key(item) for item in mapped]
        if None in normalized_mapped or len(set(normalized_mapped)) != len(normalized_mapped):
            raise ValueError(f'{name}: invalid or duplicate lock reference')
        mapped_set = set(normalized_mapped)
        for ref in sorted(refs - mapped_set):
            findings.append(f'MISSING-DIRECT-EDGE {name} {ref}')
        for ref in sorted(mapped_set - depkeys):
            findings.append(f'MISSING-RECORD {name} {ref}')
        if not refs and not local_calls and not unsupported and not mapped_set:
            notes.append(f'NO-USES-MAPPED {name}')
        for target in local_calls:
            notes.append(f'LOCAL-REUSABLE {name} -> {target} (callee checked separately)')
        for ref in sorted(mapped_set - refs):
            notes.append(f'EXTRA-OR-TRANSITIVE-EDGE {name} {ref} (source review needed)')
    return findings, notes


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('root', nargs='?', default='.')
    parser.add_argument('--unmanaged', action='append', default=[], help='explicit workflow path excluded by an owner; still requires manual closure review')
    args = parser.parse_args()
    try:
        findings, notes = inspect(args.root, args.unmanaged)
    except (OSError, ValueError, TypeError, yaml.YAMLError) as exc:
        print(f'NOT-RUN: {exc}', file=sys.stderr)
        return 2
    for item in notes:
        print('NOTE:', item)
    for item in findings:
        print('FINDING:', item)
    print(f'workflow-map findings={len(findings)}; remote source closure and provenance NOT VERIFIED')
    if findings:
        return 1
    return 2 if args.unmanaged else 0


if __name__ == '__main__':
    sys.exit(main())
