#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# Offline configuration tests for workflow pin updates. Requires Ruby with Psych.
# Usage: bash scripts/tests/workflow-pin-contract-test.sh
set -euo pipefail
cd "$(dirname "${BASH_SOURCE[0]}")/../.."

ruby - <<'RUBY'
require 'json'
require 'psych'

workflow_dir = '.github/workflows'
lock = Psych.safe_load_file("#{workflow_dir}/actions.lock")
sources = JSON.parse(File.read('scripts/lib/pinned-reusable-sources.json')).fetch('sources')
load_jobs = ->(path) { Psych.safe_load_file(path, aliases: true).fetch('jobs') }

# These commits are the targets of the reviewed SHA pins before the switch to
# tags. A tag and lock entry agreeing with each other cannot prove that the
# switch kept the same target. Update this evidence after reviewing a new target.
reviewed_commits = {
  'actions/cache' => '55cc8345863c7cc4c66a329aec7e433d2d1c52a9',
  'actions/checkout' => '3d3c42e5aac5ba805825da76410c181273ba90b1',
  'actions/download-artifact' => '9000827ccba6bdab643e8b6fd33ac0654aef8333',
  'actions/upload-artifact' => 'cf430e030ddbb5b0abf93d22962f4752f3646cd9',
  'step-security/harden-runner' => '1d88384fd173af6ecb012dfd0264a306b711241a'
}

failures = 0
check = lambda do |label, actual, expected|
  if actual != expected
    warn "FAIL: #{label}: expected #{expected.inspect}, got #{actual.inspect}"
    failures += 1
  end
end

{
  'fleet-report-back-reusable.yml' => { 'actions/cache' => 2, 'actions/checkout' => 2, 'actions/upload-artifact' => 1 },
  'fleet-report-back.yml' => { 'actions/checkout' => 2, 'actions/download-artifact' => 1 },
  'inbox-steward.yml' => { 'step-security/harden-runner' => 1 },
  'instant-sync.yml' => { 'step-security/harden-runner' => 1 },
  'push-email-notify.yml' => { 'step-security/harden-runner' => 1 },
  'supervised-fleet-scan.yml' => { 'step-security/harden-runner' => 1 }
}.each do |file, counts|
  path = "#{workflow_dir}/#{file}"
  uses = load_jobs.call(path).values.flat_map { |job| job.fetch('steps', []) }
                 .filter_map { |step| step['uses'] }
  locked = lock.fetch('workflows').fetch(path)

  counts.each do |action, count|
    refs = uses.select { |ref| ref.start_with?("#{action}@") }
    label = "#{file}: #{action}"
    check.call("#{label} step count", refs.length, count)
    # Multiple steps may use the same action, but resolve through one lock entry.
    check.call("#{label} distinct refs", refs.uniq.length, 1)
    check.call("#{label} workflow lock entries", locked.select { |ref| ref.start_with?("#{action}@") }, refs.uniq)
    refs.uniq.each do |key|
      tag = key.split('@', 2).last
      entry = lock.fetch('dependencies').fetch(key, {})
      check.call("#{label} uses a release tag", tag.match?(/\Av\d+\.\d+\.\d+\z/), true)
      check.call("#{label} dependency ref", entry['ref'], tag)
      check.call("#{label} resolved commit", entry['commit'], "sha1-#{reviewed_commits.fetch(action)}")
    end
  end
end

{
  'hypatia-scan.yml' => 'scanner-credential',
  'secret-scanner.yml' => 'scan'
}.each do |file, job|
  path = "#{workflow_dir}/#{file}"
  active = sources.select do |source|
    source['role'] == 'active' && source.fetch('callers').any? do |caller|
      caller.values_at('workflow', 'job') == [path, job]
    end
  end
  check.call("#{file}: #{job} active source count", active.length, 1)
  next unless active.one?

  repository, source_path, commit = active.first.values_at('repository', 'path', 'commit')
  check.call("#{file}: #{job} reviewed source uses a full SHA", commit.match?(/\A[0-9a-f]{40}\z/), true)
  check.call("#{file}: #{job} reviewed reusable pin", load_jobs.call(path).fetch(job)['uses'],
             "#{repository}/#{source_path}@#{commit}")
end

abort "workflow pin contracts: #{failures} failure(s)" unless failures.zero?
puts 'workflow pin contracts passed'
RUBY
