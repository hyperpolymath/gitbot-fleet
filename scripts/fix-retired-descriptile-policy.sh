#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# Repair only a finding's policy file, after its canonical targets exist.
set -euo pipefail
REPO_PATH="${1:?Usage: $0 <repo-path> <finding-json>}"
FINDING_JSON="${2:?Missing finding JSON file}"
ruby - "$REPO_PATH" "$FINDING_JSON" <<'RUBY'
require 'json'
require 'pathname'

root = File.realpath(ARGV.fetch(0))
finding = JSON.parse(File.read(ARGV.fetch(1)))
relative = finding['file'] || finding.dig('location', 'file')
abort 'Finding must name a policy file' unless relative.is_a?(String) && !relative.empty?
path = Pathname.new(relative)
abort 'Finding path must be repository-relative' if path.absolute? || path.each_filename.include?('..')
allowed = %w[Justfile justfile].include?(relative) ||
          (path.dirname.to_s == '.github/workflows' && %w[.yml .yaml].include?(path.extname)) ||
          (%w[scripts .githooks].include?(path.dirname.to_s) && path.extname == '.sh')
target = File.join(root, relative)
unless allowed && !File.symlink?(target) && File.realpath(target).start_with?(root + '/')
  abort 'Refusing a path outside supported repository policy files'
end
pattern = %r{\.machine_readable/(?:6a2/)?(STATE|META|ECOSYSTEM|AGENTIC|NEUROSYM|PLAYBOOK|ANCHOR)\.a2ml}
content = File.read(target)
names = content.scan(pattern).flatten.uniq
names.each do |name|
  canonical = File.join(root, '.machine_readable/descriptiles', name + '.a2ml')
  unless !File.symlink?(canonical) && File.file?(canonical) && File.realpath(canonical).start_with?(root + '/')
    abort "Canonical descriptile missing or unsafe: #{name}; reconcile files before repairing policy"
  end
end
updated = content.gsub(pattern) { ".machine_readable/descriptiles/#{$1}.a2ml" }
File.write(target, updated) unless updated == content
puts "#{relative}: #{names.length} descriptile reference target(s) reconciled"
RUBY
