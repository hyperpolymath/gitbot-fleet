# SPDX-License-Identifier: MPL-2.0
# Actual delivery witness: run from the matching Hypatia checkout with
# MIX_ENV=test mix run --no-start /absolute/fleet/tests/hypatia-single-admission-test.exs
# Nickel 1.11.0 is required on PATH or at the NICKEL environment path.
# FLEET_SINGLE_ADMISSION_SOURCE optionally selects another producer revision for
# the failing-before control. The source feed is never populated by test copy.

unless Mix.env() == :test, do: raise("run this witness with MIX_ENV=test")

if Process.whereis(Hypatia.Supervisor),
  do: raise("run with --no-start and no running Hypatia application")

{:ok, _} = Application.ensure_all_started(:telemetry)
ExUnit.start(autorun: false, seed: 0)

defmodule Fleet.HypatiaSingleAdmissionTest do
  use ExUnit.Case, async: false

  alias Hypatia.{EventId, LearningScheduler, OutcomeTracker, Receipt}

  @moduletag capture_log: true
  @moduletag timeout: 180_000

  setup do
    source =
      System.get_env("FLEET_SINGLE_ADMISSION_SOURCE", Path.expand("..", __DIR__))
      |> Path.expand()

    hypatia = File.cwd!()
    real_mix = System.find_executable("mix") || raise("canonical Mix is required")
    root = Path.join(System.tmp_dir!(), "fleet-admission-#{System.unique_integer([:positive])}")
    data = Path.join(root, "data")
    fleet = Path.join(root, "fleet")
    repos = Path.join(root, "repos")
    keys = [:verisimdb_data_path, :annealing_state_path, :fleet_path]
    original = Map.new(keys, &{&1, Application.fetch_env(:hypatia, &1)})
    env_keys = ~w(PATH HYPATIA_REPOS_DIR ADMISSION_FIXTURE ADMISSION_SCAN_MODE)
    original_env = Map.new(env_keys, &{&1, System.get_env(&1)})

    on_exit(fn ->
      for {key, value} <- original do
        case value do
          {:ok, prior} -> Application.put_env(:hypatia, key, prior)
          :error -> Application.delete_env(:hypatia, key)
        end
      end

      for {key, value} <- original_env do
        if value, do: System.put_env(key, value), else: System.delete_env(key)
      end

      File.rm_rf!(root)
    end)

    for dir <- ["data/dispatch", "data/recipes", "fleet/scripts/lib", "repos/sample", "bin"] do
      File.mkdir_p!(Path.join(root, dir))
    end

    Application.put_env(:hypatia, :verisimdb_data_path, data)
    Application.put_env(:hypatia, :annealing_state_path, Path.join(data, "annealing-states"))
    Application.put_env(:hypatia, :fleet_path, fleet)
    System.put_env("HYPATIA_REPOS_DIR", repos)
    System.put_env("ADMISSION_FIXTURE", root)
    System.put_env("PATH", Path.join(root, "bin") <> ":" <> System.fetch_env!("PATH"))

    original_runner = File.read!(Path.join(source, "scripts/dispatch-runner.sh"))
    lines = String.split(original_runner, "\n")

    index =
      Enum.find_index(lines, fn line ->
        String.starts_with?(line, "printf ") and
          String.contains?(line, "BLOCKED: repository directive enforcement is not qualified")
      end)

    assert is_integer(index), "production quarantine marker must exist"
    assert Enum.at(lines, index + 1) == "exit 78"
    runner = Path.join(fleet, "scripts/dispatch-runner.sh")

    # The established test seam removes exactly the two unconditional interlock
    # lines in a disposable copy. Production has no bypass flag.
    isolated = lines |> List.delete_at(index + 1) |> List.delete_at(index)
    File.write!(runner, Enum.join(isolated, "\n"))
    assert length(lines) - length(isolated) == 2

    for path <- ["scripts/repo-path-overrides.json", "scripts/lib/outcome-verdict.sh"] do
      File.cp!(Path.join(source, path), Path.join(fleet, path))
    end

    executable!(Path.join(root, "bin/mix"), ~S"""
    #!/usr/bin/env bash
    set -euo pipefail
    printf '%s\n' "$*" >> "$ADMISSION_FIXTURE/mix-calls"
    case "$*" in
      'help hypatia.record_outcome') printf '%s\n' '--verdict'; exit 0 ;;
      hypatia.event_id) exec "$ADMISSION_REAL_MIX" "$@" ;;
      hypatia.record_outcome*) printf '%s\n' forbidden >> "$ADMISSION_FIXTURE/direct-calls"; exit 99 ;;
      *) printf '%s\n' 'unexpected Mix command' >&2; exit 98 ;;
    esac
    """)

    executable!(Path.join(fleet, "scripts/fix-admission-fixture.sh"), ~S"""
    #!/usr/bin/env bash
    # outcome-predicate: --check
    set -euo pipefail
    if [[ "$1" == --check ]]; then
      if [[ -e "$2/needs-fix" ]]; then echo 1; else echo 0; fi
      exit 0
    fi
    jq -e '.kind == "start" and .producer == "dispatch-runner"' \
      "$ADMISSION_FIXTURE/fleet/shared-context/learning/fix-attempts.jsonl" >/dev/null
    printf '%s\n' executed >> "$ADMISSION_FIXTURE/fix-calls"
    if [[ "$ADMISSION_FIX_MODE" == failure ]]; then exit 7; fi
    rm -f "$1/needs-fix"
    """)

    executable!(Path.join(root, "bin/panic-attack"), ~S"""
    #!/usr/bin/env bash
    set -euo pipefail
    [[ "$1" == assail && "$2" == "$ADMISSION_FIXTURE/repos/sample" ]]
    printf '%s\n' "$2" >> "$ADMISSION_FIXTURE/scan-calls"
    if [[ "$ADMISSION_SCAN_MODE" == clean ]]; then
      printf '%s\n' '{"weak_points":[]}'
    else
      printf '%s\n' '{"weak_points":[{"category":"FixtureCategory","file":"fixture.txt","line":1}]}'
    fi
    """)

    File.write!(
      Path.join(data, "recipes/recipe-admission-fixture.json"),
      Jason.encode!(%{
        "id" => "recipe-admission-fixture",
        "confidence" => 0.5,
        "target_categories" => ["FixtureCategory"]
      })
    )

    File.write!(
      Path.join(data, "dispatch/pending.jsonl"),
      Jason.encode!(%{
        "repo" => "sample",
        "tier" => "eliminate",
        "strategy" => "auto_execute",
        "pattern_id" => "fixture",
        "recipe_id" => "recipe-admission-fixture",
        "fix_script" => "fix-admission-fixture.sh",
        "auto_fixable" => true
      }) <> "\n"
    )

    env = [
      {"FLEET_ROOT", fleet},
      {"REPOS_BASE", repos},
      {"HYPATIA_FLEET_PATH", fleet},
      {"HYPATIA_DATA", data},
      {"HYPATIA_VERISIM_DATA_PATH", data},
      {"VERISIMDB_DATA", Path.join(root, "mirror")},
      {"HYPATIA_HOME", hypatia},
      {"HYPATIA_OUTCOME_REPORT", "on"},
      {"KIN_DIR", Path.join(root, "kin")},
      {"ADMISSION_REAL_MIX", real_mix},
      {"GITHUB_TOKEN", ""},
      {"FLEET_DISPATCH_TOKEN", ""},
      {"BASH_ENV", ""},
      {"ENV", ""}
    ]

    %{root: root, data: data, fleet: fleet, repos: repos, runner: runner, env: env}
  end

  test "actual delivery admits each attempt once and never attributes an unbound scan", ctx do
    run_attempt(ctx, "failure")
    run_attempt(ctx, "failure")
    source = Path.join(ctx.fleet, "shared-context/learning/fix-outcomes.jsonl")

    assert File.regular?(source), "producer must write the actual configured import feed"
    assert stored(ctx.data) == [], "producer must not write raw rows into the learning ledger"

    refute File.exists?(Path.join(ctx.root, "direct-calls")),
           "identified attempts must not use the direct recorder"

    assert [first, second] = read_rows(source)
    assert first["attempt_contract"] == "fleet-attempt/1"
    assert first["repo_relative_path"] == "sample"
    assert EventId.valid?(first["event_id"])
    assert EventId.valid?(second["event_id"])
    refute first["event_id"] == second["event_id"]
    assert {2, _signature} = LearningScheduler.ingest_fleet_outcomes(nil)
    assert counts(ctx.data) == %{successes: 0, failures: 2, false_positives: 0, ignored: 0}
    refute File.exists?(Path.join(ctx.root, "scan-calls")), "qualified failures require no rescan"

    for {mode, expected} <- [{"clean", "verified"}, {"adverse", "still_present"}] do
      System.put_env("ADMISSION_SCAN_MODE", mode)
      run_attempt(ctx, "success")
      assert {0, _signature} = LearningScheduler.ingest_fleet_outcomes(nil)
      row = stored(ctx.data) |> List.last()
      assert row["admission_version"] == "fleet-attempt/1"
      assert row["rescan_observation"]["binding"] == "unbound"
      assert row["rescan_observation"]["result"] == expected
      assert row["rescan_observation"]["attempt_event_id"] == row["event_id"]

      source_row =
        read_lines(source)
        |> Enum.find(&(Jason.decode!(&1)["event_id"] == row["event_id"]))

      assert is_binary(source_row)
      assert row["producer_source"]["row_sha256"] == sha256(source_row)
      assert row["rescan_observation"]["source_row_sha256"] == sha256(source_row)
      assert EventId.valid?(row["rescan_observation"]["observation_id"])
      refute row["rescan_observation"]["observation_id"] == row["event_id"]
      refute OutcomeTracker.qualified_outcome?(row)
    end

    rows = stored(ctx.data)
    assert length(rows) == 4
    assert counts(ctx.data) == %{successes: 0, failures: 2, false_positives: 0, ignored: 2}
    assert length(read_lines(Path.join(ctx.root, "scan-calls"))) == 2
    assert length(read_lines(Path.join(ctx.root, "fix-calls"))) == 4
    refute File.exists?(Path.join(ctx.root, "direct-calls"))
    assert Enum.all?(rows, &(&1["source_kind"] == "fleet_import"))

    source_bytes = File.read!(source)
    month = Date.utc_today() |> Date.to_iso8601() |> String.slice(0, 7)

    for mirror <- [ctx.data, Path.join(ctx.root, "mirror")] do
      assert File.read!(Path.join([mirror, "dispatch/fleet-outcomes", month <> ".jsonl"])) ==
               source_bytes
    end

    receipts = Receipt.all(ctx.data)
    assert length(receipts) == 4
    assert Enum.count(receipts, &(&1["verdict"] == "credited_failure")) == 2
    assert Enum.count(receipts, &(&1["verdict"] == "audit_only")) == 2

    event_ids = read_rows(source) |> Enum.map(& &1["event_id"]) |> Enum.sort()
    assert length(Enum.uniq(event_ids)) == 4
    assert Enum.all?(event_ids, &EventId.valid?/1)
    assert Enum.sort(Enum.map(rows, & &1["event_id"])) == event_ids
    assert Enum.sort(Enum.map(receipts, & &1["event_id"])) == event_ids

    starts = read_rows(Path.join(ctx.fleet, "shared-context/learning/fix-attempts.jsonl"))
    assert Enum.sort(Enum.map(starts, & &1["event_id"])) == event_ids

    for receipt <- receipts do
      assert receipt["source"]["kind"] == "fleet_import"
      assert String.starts_with?(receipt["source"]["log"], "outcomes/")
      line = String.to_integer(receipt["source"]["line"])
      bytes = read_lines(Path.join(ctx.data, receipt["source"]["log"])) |> Enum.at(line - 1)
      assert receipt["proof_or_test_ref"]["row_sha256"] == sha256(bytes)
      assert Jason.decode!(bytes)["event_id"] == receipt["event_id"]
      assert receipt["parent_event_ids"] == []
      assert "rescan" in receipt["missing_evidence"]
      check_receipt_contract!(ctx.root, receipt)
    end

    # Replayed transport is an injected duplicate control, not setup needed to
    # deliver the original events: all four were imported directly above.
    before = snapshot(ctx.data)
    File.write!(source, source_bytes, [:append])
    assert {0, signature} = LearningScheduler.ingest_fleet_outcomes(nil)
    assert {0, _} = LearningScheduler.ingest_fleet_outcomes(signature)
    assert {0, _} = LearningScheduler.ingest_fleet_outcomes(nil)
    assert snapshot(ctx.data) == before
    assert length(read_lines(Path.join(ctx.root, "scan-calls"))) == 2
  end

  defp run_attempt(ctx, mode) do
    File.write!(Path.join(ctx.repos, "sample/needs-fix"), "fixture\n")

    {output, status} =
      System.cmd("bash", [ctx.runner, "--limit", "1", "--parallel", "1"],
        env: [{"ADMISSION_FIX_MODE", mode} | ctx.env],
        stderr_to_stdout: true
      )

    assert status == 0, "fixture runner failed: #{output}"
  end

  defp executable!(path, source) do
    File.write!(path, source)
    File.chmod!(path, 0o700)
  end

  defp read_lines(path), do: path |> File.read!() |> String.split("\n", trim: true)
  defp read_rows(path), do: path |> read_lines() |> Enum.map(&Jason.decode!/1)

  defp stored(data) do
    Path.wildcard(Path.join(data, "outcomes/*.jsonl"))
    |> Enum.sort()
    |> Enum.flat_map(&read_rows/1)
  end

  defp counts(data), do: OutcomeTracker.qualified_outcome_counts(stored(data))
  defp sha256(bytes), do: :crypto.hash(:sha256, bytes) |> Base.encode16(case: :lower)

  defp check_receipt_contract!(root, receipt) do
    nickel =
      System.get_env("NICKEL") || System.find_executable("nickel") ||
        raise("Nickel 1.11.0 is required to check the actual admission receipts")

    json = Path.join(root, "receipt-contract.json")
    check = Path.join(root, "receipt-contract.ncl")
    schema = Path.expand("schema/receipt.ncl")
    File.write!(json, Jason.encode!(receipt))

    File.write!(
      check,
      "let s = import #{Jason.encode!(schema)} in (import #{Jason.encode!(json)}) | s.Receipt\n"
    )

    {output, status} =
      System.cmd(nickel, ["export", "--format", "json", check], stderr_to_stdout: true)

    assert status == 0, "actual admission receipt failed the existing Nickel contract: #{output}"
  end

  defp snapshot(data) do
    Path.wildcard(Path.join(data, "**/*"))
    |> Enum.filter(&File.regular?/1)
    |> Map.new(&{Path.relative_to(&1, data), File.read!(&1)})
  end
end

result = ExUnit.run()
System.halt(if(result.failures == 0, do: 0, else: 1))
