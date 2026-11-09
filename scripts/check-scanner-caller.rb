#!/usr/bin/env ruby
# SPDX-License-Identifier: MPL-2.0
# Fail closed on changes to the fleet scanner's deployed security contract.
require 'psych'

path = ARGV.fetch(0, '.github/workflows/secret-scanner.yml')
expected = 'hyperpolymath/proven-cicd/.github/workflows/secret-scanner-reusable.yml@03e858b6015e67cc1f9bc31ac1e29ddc1b6fd409'

# Validate a Psych node and its descendants before loading the workflow.
# Raises RuntimeError for aliases, non-scalar mapping keys, or duplicate keys
# within a mapping. The caller reports these failures and exits with status 1.
def validate_nodes(node)
  raise 'YAML aliases are not supported' if node.is_a?(Psych::Nodes::Alias)
  if node.is_a?(Psych::Nodes::Mapping)
    keys = node.children.each_slice(2).map do |key, _value|
      raise 'non-scalar YAML key' unless key.is_a?(Psych::Nodes::Scalar)
      key.value
    end
    raise 'duplicate YAML key' unless keys.uniq.length == keys.length
  end
  node.children&.each { |child| validate_nodes(child) }
end

# Require Ruby equality with the reviewed value; returns nil on success.
# Raises RuntimeError on mismatch, using label to identify the contract field.
# The caller reports the failure and exits with status 1.
def require_equal(actual, expected, label)
  raise "#{label} does not match the reviewed contract" unless actual == expected
end

begin
  tree = Psych.parse_file(path)
  raise 'empty YAML' unless tree
  validate_nodes(tree)
  workflow = Psych.safe_load_file(path, aliases: false)
  raise 'workflow must be a mapping' unless workflow.is_a?(Hash)
  # Psych implements YAML 1.1, where the unquoted `on` key resolves to true.
  events = workflow.key?('on') ? workflow['on'] : workflow[true]
  require_equal(events, { 'pull_request' => nil, 'push' => { 'branches' => ['main'] } }, 'triggers')
  require_equal(workflow['permissions'], { 'contents' => 'read' }, 'workflow permissions')
  jobs = workflow['jobs']
  raise 'only the scan job is expected' unless jobs.is_a?(Hash) && jobs.keys == ['scan']
  scan = jobs.fetch('scan')
  raise 'unexpected scan job fields' unless scan.is_a?(Hash) && scan.keys.sort == %w[permissions uses with]
  require_equal(scan['permissions'], { 'contents' => 'read' }, 'job permissions')
  require_equal(scan['uses'], expected, 'suite pin')
  require_equal(scan['with'], { 'fast-pr' => false }, 'scan policy')
  puts 'scanner caller contract: exact suite pin, full-history policy, triggers, least privilege and no secrets verified'
rescue StandardError => e
  warn "scanner caller contract: #{e.message}"
  exit 1
end
