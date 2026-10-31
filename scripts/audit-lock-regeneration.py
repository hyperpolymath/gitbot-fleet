#!/usr/bin/env python3
# SPDX-License-Identifier: MPL-2.0
"""Compare reviewed closure against regeneration; does not discover remote closure."""
import re
import sys
import yaml


class UniqueLoader(yaml.SafeLoader):
    pass


def unique_mapping(loader, node, deep=False):
    """Construct a YAML mapping, raising ValueError for duplicate keys.

    Pass deep through to PyYAML's key and value construction. YAML construction
    errors and TypeError for unhashable keys propagate to the caller.
    """
    mapping = {}
    for key_node, value_node in node.value:
        item = loader.construct_object(key_node, deep=deep)
        if item in mapping:
            raise ValueError(f'duplicate YAML key: {item!r}')
        mapping[item] = loader.construct_object(value_node, deep=deep)
    return mapping


UniqueLoader.add_constructor(yaml.resolver.BaseResolver.DEFAULT_MAPPING_TAG, unique_mapping)


def key(value):
    """Normalize an owner/repository@ref key, preserving the ref's case.

    Raise ValueError if the reference is not in the supported form.
    """
    if not isinstance(value, str) or not re.fullmatch(r'[^\s/@]+/[^\s/@]+@[^\s@]+', value):
        raise ValueError(f'invalid dependency reference: {value!r}')
    repo, ref = value.split('@')
    return f'{repo.lower()}@{ref}'


def load(path):
    """Load and validate a lock file as (dependencies, workflow roots).

    Dependency keys and edges are normalized; each dependency value contains
    its ref, commit, and set of edges. Raise ValueError for invalid records,
    references, or missing targets. File, YAML, and key construction errors
    propagate to the caller.
    """
    with open(path, encoding='utf-8') as f:
        doc = yaml.load(f, Loader=UniqueLoader)
    if not isinstance(doc, dict) or not isinstance(doc.get('workflows'), dict) or not isinstance(doc.get('dependencies'), dict):
        raise ValueError(f'{path}: expected workflows and dependencies mappings')
    deps = {}
    for name, record in doc['dependencies'].items():
        k = key(name)
        if k in deps or not isinstance(record, dict):
            raise ValueError(f'{path}: duplicate or invalid record {name}')
        ref, commit = record.get('ref'), record.get('commit')
        if not isinstance(ref, str) or not re.fullmatch(r'[^\s@]+', ref) or ref.startswith('sha1-'):
            raise ValueError(f'{path}: invalid ref for {name}')
        if not isinstance(commit, str) or not re.fullmatch(r'sha1-[0-9a-f]{40}', commit):
            raise ValueError(f'{path}: invalid commit for {name}')
        named_ref = name.split('@')[1]
        if re.fullmatch(r'[0-9a-f]{40}', named_ref):
            if commit != 'sha1-' + named_ref:
                raise ValueError(f'{path}: SHA key and commit differ for {name}')
        elif named_ref != ref:
            raise ValueError(f'{path}: tag ref differs from key for {name}')
        edges = record.get('uses', [])
        if not isinstance(edges, list):
            raise ValueError(f'{path}: invalid uses for {name}')
        deps[k] = (ref, commit, {key(edge) for edge in edges})
    roots = set()
    for workflow, edges in doc['workflows'].items():
        if not isinstance(workflow, str) or not isinstance(edges, list):
            raise ValueError(f'{path}: invalid workflow {workflow!r}')
        roots.update(key(edge) for edge in edges)
    for edge in roots | set().union(*(r[2] for r in deps.values())):
        if edge not in deps:
            raise ValueError(f'{path}: missing dependency {edge}')
    return deps, roots


def audit(old_path, new_path):
    """Compare reviewed dependencies reachable from proposed workflow roots.

    For roots present in the reviewed lock, check that their reviewed
    reachable records, refs, commits, and edges remain in the proposed lock.
    Print a success summary; new sources still require independent review.
    Raise ValueError on a mismatch and propagate lock loading errors.
    """
    old, _ = load(old_path)
    new, roots = load(new_path)
    queue = list(roots & old.keys())
    seen = set()
    while queue:
        k = queue.pop()
        if k in seen:
            continue
        seen.add(k)
        if k not in new:
            raise ValueError(f'dropped reviewed dependency {k}')
        if old[k][:2] != new[k][:2]:
            raise ValueError(f'changed reviewed ref/commit {k}')
        missing = old[k][2] - new[k][2]
        if missing:
            raise ValueError(f'dropped reviewed edges from {k}: {sorted(missing)}')
        queue.extend(old[k][2])
    print(f'Preserved reviewed reachable closure ({len(seen)} records); new sources require independent review.')


if __name__ == '__main__':
    if len(sys.argv) != 3:
        sys.exit(f'usage: {sys.argv[0]} REVIEWED_LOCK PROPOSED_LOCK')
    try:
        audit(*sys.argv[1:])
    except (OSError, ValueError, yaml.YAMLError) as exc:
        sys.exit(f'lock regeneration audit: {exc}')
