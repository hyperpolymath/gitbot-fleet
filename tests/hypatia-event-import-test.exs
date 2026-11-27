# SPDX-License-Identifier: MPL-2.0
# Explicit wire-compatibility witness. Run from the pinned Hypatia checkout:
# DISPATCH_TEST_EVIDENCE_DIR=/absolute/emitted-fixture MIX_ENV=test \
#   mix run --no-start /absolute/gitbot-fleet/tests/hypatia-event-import-test.exs
# The fixture files must come from scripts/tests/dispatch-paths.sh. Copying
# their bytes into the import path below is a test transfer, not delivery code.

unless Mix.env() == :test, do: raise("run this witness with MIX_ENV=test")

if Process.whereis(Hypatia.Supervisor),
  do: raise("run this witness with --no-start; the Hypatia application must be stopped")

{:ok, _} = Application.ensure_all_started(:telemetry)
ExUnit.start(autorun: false, seed: 0)

defmodule Fleet.HypatiaEventImportTest do
  use ExUnit.Case, async: false

  alias Hypatia.{EventId, LearningScheduler, OutcomeTracker, Receipt}

  @moduletag capture_log: true

  setup do
    evidence = System.fetch_env!("DISPATCH_TEST_EVIDENCE_DIR") |> Path.expand()
    root = Path.join(System.tmp_dir!(), "fleet-wire-#{System.unique_integer([:positive])}")
    data = Path.join(root, "data")
    fleet = Path.join(root, "fleet")
    keys = [:verisimdb_data_path, :annealing_state_path, :fleet_path]
    original = Map.new(keys, &{&1, Application.fetch_env(:hypatia, &1)})

    on_exit(fn ->
      for {key, value} <- original do
        case value do
          {:ok, previous} -> Application.put_env(:hypatia, key, previous)
          :error -> Application.delete_env(:hypatia, key)
        end
      end

      File.rm_rf!(root)
    end)

    File.mkdir_p!(Path.join(data, "outcomes"))
    File.mkdir_p!(Path.join(fleet, "shared-context/learning"))
    Application.put_env(:hypatia, :verisimdb_data_path, data)
    Application.put_env(:hypatia, :annealing_state_path, Path.join(data, "annealing"))
    Application.put_env(:hypatia, :fleet_path, fleet)

    primary = File.read!(Path.join(evidence, "failure-primary.jsonl"))
    central = File.read!(Path.join(evidence, "failure-central.jsonl"))
    starts = File.read!(Path.join(evidence, "failure-starts.jsonl"))

    for bytes <- [primary, central, starts], do: assert(String.ends_with?(bytes, "\n"))

    %{
      data: data,
      fleet: fleet,
      source: Path.join(fleet, "shared-context/learning/fix-outcomes.jsonl"),
      primary: primary,
      central: central,
      starts: starts
    }
  end

  test "actual producer rows retain two distinct attempt identities and their mirror", ctx do
    assert [first, second] = decode(ctx.primary)
    assert [mirrored] = decode(ctx.central)
    assert [first_start, second_start] = decode(ctx.starts)
    assert first["verdict"] == "failed"
    assert second["verdict"] == "failed"
    assert first["outcome"] == "failure"
    assert second["outcome"] == "failure"
    assert first["recipe_id"] not in [nil, "", "none", "null"]
    assert first["recipe_id"] == second["recipe_id"]
    assert EventId.valid?(first["event_id"])
    assert EventId.valid?(second["event_id"])
    refute first["event_id"] == second["event_id"]
    assert first_start["kind"] == "start"
    assert second_start["kind"] == "start"
    assert first_start["event_id"] == first["event_id"]
    assert second_start["event_id"] == second["event_id"]
    assert mirrored == second
    assert List.last(lines(ctx.primary)) == hd(lines(ctx.central))
  end

  test "explicit transfer imports attempts once, skips starts, and preserves receipt identity",
       ctx do
    File.write!(Path.join(ctx.fleet, "shared-context/learning/fix-attempts.jsonl"), ctx.starts)
    assert {0, nil} = LearningScheduler.ingest_fleet_outcomes(nil)
    assert stored(ctx.data) == []
    assert Receipt.all(ctx.data) == []

    # Even a misplaced start must not consume the later outcome's identity.
    File.write!(ctx.source, ctx.starts)
    assert {0, _signature} = LearningScheduler.ingest_fleet_outcomes(nil)
    assert stored(ctx.data) == []

    # This deliberate test transfer is the boundary being exercised. The
    # runner's current monthly-log paths do not perform this operation.
    File.write!(ctx.source, ctx.starts <> ctx.primary <> ctx.central)
    assert {2, signature} = LearningScheduler.ingest_fleet_outcomes(nil)
    rows = stored(ctx.data)
    expected_ids = ctx.primary |> decode() |> Enum.map(& &1["event_id"]) |> Enum.sort()
    assert Enum.sort(Enum.map(rows, & &1["event_id"])) == expected_ids

    assert Enum.all?(rows, fn row ->
             row["event_id_source"] == "producer" and
               row["fleet_import_id"] == row["event_id"] and
               row["verification"] == "failed"
           end)

    assert OutcomeTracker.qualified_outcome_counts(rows) == %{
             successes: 0,
             failures: 2,
             false_positives: 0,
             ignored: 0
           }

    receipts = Receipt.all(ctx.data)
    assert Enum.sort(Enum.map(receipts, & &1["event_id"])) == expected_ids
    assert Enum.all?(receipts, &(&1["verdict"] == "credited_failure"))
    assert Enum.all?(receipts, &(&1["provenance"]["event_id_source"] == "producer"))

    before_replay = snapshot(ctx.data)
    assert {0, _} = LearningScheduler.ingest_fleet_outcomes(signature)
    assert {0, _} = LearningScheduler.ingest_fleet_outcomes(nil)
    assert snapshot(ctx.data) == before_replay
  end

  test "raw monthly producer rows do not implicitly become qualified fleet imports", ctx do
    month = Date.utc_today() |> Date.to_iso8601() |> String.slice(0, 7)
    File.write!(Path.join([ctx.data, "outcomes", month <> ".jsonl"]), ctx.primary)

    assert {0, nil} = LearningScheduler.ingest_fleet_outcomes(nil)

    assert OutcomeTracker.qualified_outcome_counts(stored(ctx.data)) == %{
             successes: 0,
             failures: 0,
             false_positives: 0,
             ignored: 2
           }

    assert Receipt.all(ctx.data) == []
  end

  defp lines(bytes), do: String.split(bytes, "\n", trim: true)
  defp decode(bytes), do: bytes |> lines() |> Enum.map(&Jason.decode!/1)

  defp stored(data) do
    Path.wildcard(Path.join(data, "outcomes/*.jsonl"))
    |> Enum.sort()
    |> Enum.flat_map(&(File.read!(&1) |> decode()))
  end

  defp snapshot(data) do
    Path.wildcard(Path.join(data, "**/*"))
    |> Enum.filter(&File.regular?/1)
    |> Map.new(&{Path.relative_to(&1, data), File.read!(&1)})
  end
end

result = ExUnit.run()
System.halt(if(result.failures == 0, do: 0, else: 1))
