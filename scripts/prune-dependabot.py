#!/usr/bin/env python3
import sys
import os
import yaml

if len(sys.argv) < 2:
    print("Usage: prune-dependabot.py <repo-path>")
    sys.exit(1)

repo_path = sys.argv[1]
dependabot_yml = os.path.join(repo_path, '.github', 'dependabot.yml')
dependabot_yaml = os.path.join(repo_path, '.github', 'dependabot.yaml')

file_to_process = None
if os.path.exists(dependabot_yml):
    file_to_process = dependabot_yml
elif os.path.exists(dependabot_yaml):
    file_to_process = dependabot_yaml

if not file_to_process:
    sys.exit(0)

try:
    with open(file_to_process, 'r') as f:
        config = yaml.safe_load(f)
except Exception as e:
    print(f"Failed to load yaml: {e}")
    sys.exit(0)

if not config or 'updates' not in config:
    sys.exit(0)

def has_manifest(repo, ecosystem):
    if ecosystem == 'cargo':
        return os.path.exists(os.path.join(repo, 'Cargo.toml'))
    if ecosystem == 'mix':
        return os.path.exists(os.path.join(repo, 'mix.exs'))
    if ecosystem == 'npm':
        return os.path.exists(os.path.join(repo, 'package.json'))
    if ecosystem == 'bundler':
        return os.path.exists(os.path.join(repo, 'Gemfile'))
    if ecosystem == 'pip':
        return os.path.exists(os.path.join(repo, 'requirements.txt')) or os.path.exists(os.path.join(repo, 'setup.py'))
    if ecosystem == 'gomod':
        return os.path.exists(os.path.join(repo, 'go.mod'))
    if ecosystem == 'github-actions':
        return True
    return False

original_updates = config['updates']
new_updates = []
changed = False

for update in original_updates:
    ecosystem = update.get('package-ecosystem')
    if has_manifest(repo_path, ecosystem):
        new_updates.append(update)
    else:
        changed = True
        print(f"Pruned missing ecosystem: {ecosystem}")

if changed:
    config['updates'] = new_updates
    with open(file_to_process, 'w') as f:
        yaml.dump(config, f, sort_keys=False)
    print("dependabot.yml updated.")
