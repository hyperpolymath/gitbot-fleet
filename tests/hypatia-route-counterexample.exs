# SPDX-License-Identifier: MPL-2.0
# Separate, deliberately failing gate against naive dual-route wiring: one
# emitted attempt can be credited by both the direct Mix callback and a later
# explicit fleet import. This is a counterexample to joining the current routes,
# not a universal acceptance test for a future exclusive-ingress design.
# Run from Hypatia with MIX_ENV=test, --no-start and DISPATCH_TEST_EVIDENCE_DIR
# pointing at the real producer fixture from scripts/tests/dispatch-paths.sh.

unless Mix.env() == :test, do: raise("run this witness with MIX_ENV=test")

if Process.whereis(Hypatia.Supervisor),
  do: raise("run this witness with --no-start; the Hypatia application must be stopped")

{:ok, _} = Application.ensure_all_started(:telemetry)

defmodule Fleet.HypatiaRouteCounterexample do
  alias Hypatia.{EventId, LearningScheduler, OutcomeTracker, Receipt}

  def run do
    evidence = System.fetch_env!("DISPATCH_TEST_EVIDENCE_DIR") |> Path.expand()
    [line | _] = File.read!(Path.join(evidence, "failure-primary.jsonl")) |> lines()
    row = Jason.decode!(line)
    true = row["verdict"] == "failed" and row["outcome"] == "failure"
    true = EventId.valid?(row["event_id"])
    root = Path.join(System.tmp_dir!(), "fleet-route-#{System.unique_integer([:positive])}")
    data = Path.join(root, "data")
    fleet = Path.join(root, "fleet")
    keys = [:verisimdb_data_path, :annealing_state_path, :fleet_path]
    original = Map.new(keys, &{&1, Application.fetch_env(:hypatia, &1)})

    try do
      File.mkdir_p!(Path.join(data, "outcomes"))
      Application.put_env(:hypatia, :verisimdb_data_path, data)
      Application.put_env(:hypatia, :annealing_state_path, Path.join(data, "annealing"))
      Application.put_env(:hypatia, :fleet_path, fleet)
      file = if row["file"] in [nil, ""], do: ".", else: row["file"]

      # The actual task called by dispatch-runner, with that attempt's actual
      # labels. The task deliberately does not accept producer identity.
      Mix.Tasks.Hypatia.RecordOutcome.run([
        "--recipe",
        row["recipe_id"],
        "--repo",
        row["repo"],
        "--file",
        file,
        "--outcome",
        "failure",
        "--verdict",
        "failed",
        "--format",
        "json"
      ])

      [local] = stored(data)
      true = local["event_id"] != row["event_id"]
      true = local["source_kind"] == "local_caller"
      before = OutcomeTracker.qualified_outcome_counts([local])
      true = before.failures == 1

      # The callback's bot=hypatia echo is ignored; the duplicate below is
      # caused by explicitly adding the real producer row, not by the echo.
      {0, _} = LearningScheduler.ingest_fleet_outcomes(nil)
      source = Path.join(fleet, "shared-context/learning/fix-outcomes.jsonl")
      File.write!(source, line <> "\n", [:append])
      {1, _} = LearningScheduler.ingest_fleet_outcomes(nil)
      rows = stored(data)
      after_import = OutcomeTracker.qualified_outcome_counts(rows)

      IO.puts(
        Jason.encode!(%{
          check: "one qualified contribution per emitted attempt",
          emitted_attempts: 1,
          producer_event_id: row["event_id"],
          direct_event_id: local["event_id"],
          qualified_before_import: before.failures,
          qualified_after_import: after_import.failures,
          receipts_after_import: length(Receipt.all(data)),
          automatic_delivery_tested: false
        })
      )

      if after_import.failures == 1 do
        IO.puts("PASS: one attempt has one qualified contribution across the two routes")
        0
      else
        IO.puts(
          :stderr,
          "FAIL: one actual producer attempt has #{after_import.failures} qualified " <>
            "contributions after direct recording plus explicit import; route contract is unresolved"
        )

        1
      end
    after
      for {key, value} <- original do
        case value do
          {:ok, previous} -> Application.put_env(:hypatia, key, previous)
          :error -> Application.delete_env(:hypatia, key)
        end
      end

      File.rm_rf!(root)
    end
  end

  defp lines(bytes), do: String.split(bytes, "\n", trim: true)

  defp stored(data) do
    Path.wildcard(Path.join(data, "outcomes/*.jsonl"))
    |> Enum.sort()
    |> Enum.flat_map(&(File.read!(&1) |> lines()))
    |> Enum.map(&Jason.decode!/1)
  end
end

System.halt(Fleet.HypatiaRouteCounterexample.run())
