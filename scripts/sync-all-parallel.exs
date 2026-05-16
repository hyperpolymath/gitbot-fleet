# SPDX-License-Identifier: PMPL-1.0-or-later
#
# sync-all-parallel.exs — Massively parallel repository synchronization
#
# Elixir rewrite of sync-all.sh using BEAM concurrency.
# Phase 1 (fetch/pull/push) runs fully parallel across all repos.
# Phase 2/3 (uncommitted/merge) are interactive and sequential.
#
# Usage:
#   elixir sync-all-parallel.exs [OPTIONS]
#
# Options:
#   --repos-dir PATH   Base directory (default: /var$REPOS_DIR)
#   --dry-run          Show what would happen
#   --auto             Non-interactive mode (skip all issues)
#   --concurrency N    Max concurrent git operations (default: 32)
#   --no-fetch         Skip fetching
#   --report PATH      Write report to file
#   --commit-all MSG   Auto-commit all dirty repos with given message
#   --push             Auto-push after commit (with --commit-all)
#   --batch-size N     Repos processed per throttled batch (default: 25,
#                      env SYNC_BATCH_SIZE). Set 0 to disable batching.
#   --batch-pause SEC  Seconds to pause between batches (default: 45,
#                      env SYNC_BATCH_PAUSE_SEC). A small random jitter is
#                      added on top to de-correlate CI trigger waves.
#   --no-throttle      Disable inter-batch pacing entirely (one big stream,
#                      legacy behaviour). Implied by --dry-run (no pushes).
#
# THROTTLING / THUNDERING-HERD CONTROL
#   Phase 1 pushes to every owned repo. Each push fires that repo's GitHub
#   Actions workflows. Pushing the whole estate (~355 repos) in one tight
#   window saturates the account-wide hosted-runner concurrency cap, leaving
#   a large fraction of estate CI transiently `queued`. To avoid this we
#   process repos in batches of --batch-size and pause --batch-pause seconds
#   (plus jitter) between batches, so CI trigger waves are spread over time.
#   Within a batch, the original --concurrency parallelism is unchanged, and
#   a failure in one repo never aborts the batch or the run.

defmodule SyncAll do
  @owned_prefixes [
    "github.com/hyperpolymath",
    "github.com:hyperpolymath",
    "gitlab.com/hyperpolymath",
    "gitlab.com:hyperpolymath",
    "bitbucket.org/hyperpolymath",
    "bitbucket.org:hyperpolymath-dev"
  ]

  @ignored_dirs ~w[logs monitoring .git-private-farm]

  # Throttling defaults. batch_size 25 + 45s pause keeps the per-minute
  # workflow-trigger rate well under the account hosted-runner cap while
  # still syncing the whole estate in a bounded time. Env vars let ops tune
  # without editing the script; CLI flags override env.
  @default_batch_size (case Integer.parse(System.get_env("SYNC_BATCH_SIZE") || "") do
                          {n, _} when n >= 0 -> n
                          _ -> 25
                        end)
  @default_batch_pause_sec (case Integer.parse(System.get_env("SYNC_BATCH_PAUSE_SEC") || "") do
                              {n, _} when n >= 0 -> n
                              _ -> 45
                            end)

  defstruct [
    :repos_dir,
    :dry_run,
    :auto_mode,
    :concurrency,
    :do_fetch,
    :report_file,
    :commit_msg,
    :auto_push,
    :batch_size,
    :batch_pause_sec,
    :throttle,
    max_depth: 3
  ]

  # --- Entry Point ---

  def main(args) do
    config = parse_args(args)
    IO.puts("\n\e[1mSync All Parallel (Elixir/BEAM)\e[0m")
    IO.puts("  Repos dir:   #{config.repos_dir}")
    IO.puts("  Concurrency: #{config.concurrency}")
    IO.puts("  Dry run:     #{config.dry_run}")

    cond do
      not throttling_active?(config) ->
        IO.puts("  Throttle:    off (no inter-batch pacing)")

      true ->
        IO.puts("  Throttle:    batches of #{config.batch_size}, ~#{config.batch_pause_sec}s pause (+jitter)")
    end

    if config.commit_msg, do: IO.puts("  Commit msg:  #{config.commit_msg}")

    start = System.monotonic_time(:millisecond)

    # Discovery
    {repos, submodules} = discover_repos(config)
    IO.puts("\nFound \e[32m#{length(repos)}\e[0m repositories (skipped #{length(submodules)} submodule(s))")

    for sm <- submodules, do: IO.puts("  \e[2msubmodule: #{sm}\e[0m")

    # Phase 1: Parallel fetch/pull/push
    results = phase1_parallel(repos, config)
    phase1_summary(results, config)

    # Phase 2: Uncommitted changes
    dirty = Enum.filter(results, fn r -> r.dirty end)
    phase2_uncommitted(dirty, config)

    # Phase 3: Diverged
    diverged = Enum.filter(results, fn r -> r.diverged end)
    phase3_diverged(diverged, config)

    elapsed = System.monotonic_time(:millisecond) - start
    elapsed_s = div(elapsed, 1000)
    IO.puts("\n\e[1m========================================\e[0m")
    IO.puts("\e[1m Sync Complete! (#{div(elapsed_s, 60)}m#{rem(elapsed_s, 60) |> Integer.to_string() |> String.pad_leading(2, "0")}s)\e[0m")
    IO.puts("\e[1m========================================\e[0m")

    clean_count = Enum.count(results, fn r -> not r.dirty end)
    fetch_count = Enum.count(results, fn r -> r.fetched end)
    pull_count = Enum.count(results, fn r -> r.pulled end)
    push_count = Enum.count(results, fn r -> r.pushed end)

    IO.puts("  Repos:    #{length(repos)}")
    IO.puts("  Clean:    #{clean_count}")
    IO.puts("  Fetched:  #{fetch_count}")
    IO.puts("  Pulled:   #{pull_count}")
    IO.puts("  Pushed:   #{push_count}")

    if config.report_file do
      write_report(config.report_file, results, repos, elapsed_s)
      IO.puts("  Report:   #{config.report_file}")
    end
  end

  # --- Argument Parsing ---

  defp parse_args(args) do
    parse_args(args, %SyncAll{
      repos_dir: "/var$REPOS_DIR",
      dry_run: false,
      auto_mode: false,
      concurrency: 32,
      do_fetch: true,
      report_file: nil,
      commit_msg: nil,
      auto_push: false,
      batch_size: @default_batch_size,
      batch_pause_sec: @default_batch_pause_sec,
      throttle: true
    })
  end

  defp parse_args([], config), do: config
  defp parse_args(["--dry-run" | rest], config), do: parse_args(rest, %{config | dry_run: true})
  defp parse_args(["--auto" | rest], config), do: parse_args(rest, %{config | auto_mode: true})
  defp parse_args(["--no-fetch" | rest], config), do: parse_args(rest, %{config | do_fetch: false})
  defp parse_args(["--push" | rest], config), do: parse_args(rest, %{config | auto_push: true})
  defp parse_args(["--repos-dir", dir | rest], config), do: parse_args(rest, %{config | repos_dir: dir})
  defp parse_args(["--concurrency", n | rest], config), do: parse_args(rest, %{config | concurrency: String.to_integer(n)})
  defp parse_args(["--report", path | rest], config), do: parse_args(rest, %{config | report_file: path})
  defp parse_args(["--commit-all", msg | rest], config), do: parse_args(rest, %{config | commit_msg: msg})
  defp parse_args(["--depth", n | rest], config), do: parse_args(rest, %{config | max_depth: String.to_integer(n)})
  defp parse_args(["--batch-size", n | rest], config), do: parse_args(rest, %{config | batch_size: String.to_integer(n)})
  defp parse_args(["--batch-pause", n | rest], config), do: parse_args(rest, %{config | batch_pause_sec: String.to_integer(n)})
  defp parse_args(["--no-throttle" | rest], config), do: parse_args(rest, %{config | throttle: false})

  defp parse_args([arg | rest], config) do
    if File.dir?(arg), do: parse_args(rest, %{config | repos_dir: arg}), else: parse_args(rest, config)
  end

  # --- Discovery ---

  defp discover_repos(config) do
    {output, 0} = System.cmd("find", [
      config.repos_dir, "-maxdepth", to_string(config.max_depth),
      "-name", ".git", "-type", "d"
    ], stderr_to_stdout: true)

    all_git_dirs =
      output
      |> String.split("\n", trim: true)
      |> Enum.sort()

    repos_and_types =
      all_git_dirs
      |> Enum.map(fn git_dir ->
        repo_dir = Path.dirname(git_dir)
        rel = Path.relative_to(repo_dir, config.repos_dir)
        {rel, repo_dir}
      end)
      |> Enum.reject(fn {rel, _} ->
        Enum.any?(@ignored_dirs, fn d -> rel == d or String.starts_with?(rel, d <> "/") end)
      end)

    {repos, submodules} =
      Enum.split_with(repos_and_types, fn {rel, _full} ->
        not is_submodule?(rel, config.repos_dir)
      end)

    {Enum.map(repos, fn {rel, _} -> rel end), Enum.map(submodules, fn {rel, _} -> rel end)}
  end

  defp is_submodule?(rel, repos_dir) do
    parts = Path.split(rel)
    num_parts = length(parts)

    # Top-level repos can't be submodules
    if num_parts <= 1 do
      false
    else
      # Walk parent directories looking for .gitmodules
      1..(num_parts - 1)//1
      |> Enum.any?(fn n ->
        parent = parts |> Enum.take(n) |> Path.join()
        parent_abs = Path.join(repos_dir, parent)
        gitmodules = Path.join(parent_abs, ".gitmodules")

        if File.exists?(gitmodules) do
          subpath = parts |> Enum.drop(n) |> Path.join()
          case File.read(gitmodules) do
            {:ok, content} -> String.contains?(content, "path = #{subpath}")
            _ -> false
          end
        else
          false
        end
      end)
    end
  end

  # --- Phase 1: Parallel Sync ---

  # Throttling is only meaningful when we will actually push (pushes are
  # what trigger remote CI). A dry run pushes nothing, so pacing it just
  # wastes wall-clock — disable it there as well as on explicit --no-throttle
  # or a non-positive batch size.
  defp throttling_active?(config) do
    config.throttle and not config.dry_run and config.batch_size > 0
  end

  defp phase1_parallel(repos, config) do
    total = length(repos)
    counter = :counters.new(1, [:atomics])

    IO.puts("\n\e[1m=== Phase 1: Parallel Fetch/Pull/Push (#{config.concurrency} workers) ===\e[0m")

    batches =
      if throttling_active?(config) do
        Enum.chunk_every(repos, config.batch_size)
      else
        [repos]
      end

    nbatches = length(batches)

    if nbatches > 1 do
      IO.puts(
        "\e[2mThrottled: #{nbatches} batch(es) of up to #{config.batch_size}, " <>
          "~#{config.batch_pause_sec}s (+jitter) between batches\e[0m"
      )
    end

    batches
    |> Enum.with_index(1)
    |> Enum.flat_map(fn {batch, idx} ->
      results = run_batch(batch, config, counter, total)

      # Pause AFTER every batch except the last, so the next wave of
      # workflow triggers lands once the previous wave has had time to
      # start draining the runner queue. Jitter de-correlates repeated runs.
      if idx < nbatches do
        jitter_ms = :rand.uniform(5_000)
        pause_ms = config.batch_pause_sec * 1_000 + jitter_ms
        IO.puts("\n\e[2m  Batch #{idx}/#{nbatches} done — pausing #{div(pause_ms, 1000)}s before next batch…\e[0m")
        Process.sleep(pause_ms)
      end

      results
    end)
    |> tap(fn _ -> IO.puts("") end)  # Clear progress line
  end

  # Run one batch at the configured concurrency. A crash/timeout in one
  # repo is mapped to an error result and never aborts the batch or run.
  defp run_batch(batch, config, counter, total) do
    batch
    |> Task.async_stream(
      fn repo -> sync_one_repo(repo, config, counter, total) end,
      max_concurrency: config.concurrency,
      timeout: 120_000,
      on_timeout: :kill_task
    )
    |> Enum.map(fn
      {:ok, result} -> result
      {:exit, _reason} -> %{repo: "?", dirty: false, fetched: false, pulled: false, pushed: false, diverged: false, error: "timeout"}
    end)
  end

  defp sync_one_repo(repo, config, counter, total) do
    repo_path = Path.join(config.repos_dir, repo)
    result = %{repo: repo, dirty: false, fetched: false, pulled: false, pushed: false, diverged: false, error: nil}

    # Check dirty
    dirty = repo_is_dirty?(repo_path)
    result = %{result | dirty: dirty}

    # Fetch
    result =
      if config.do_fetch do
        case git(repo_path, ["fetch", "origin"]) do
          {_, 0} -> %{result | fetched: true}
          _ -> result
        end
      else
        result
      end

    # Get branch info
    branch = case git(repo_path, ["rev-parse", "--abbrev-ref", "HEAD"]) do
      {b, 0} -> String.trim(b)
      _ -> nil
    end

    result =
      if branch do
        # Check if tracking branch exists
        case git(repo_path, ["show-ref", "--verify", "--quiet", "refs/remotes/origin/#{branch}"]) do
          {_, 0} ->
            {behind_s, 0} = git(repo_path, ["rev-list", "--count", "HEAD..origin/#{branch}"])
            {ahead_s, 0} = git(repo_path, ["rev-list", "--count", "origin/#{branch}..HEAD"])
            behind = behind_s |> String.trim() |> String.to_integer()
            ahead = ahead_s |> String.trim() |> String.to_integer()

            result = if behind > 0 do
              # Check if can fast-forward
              case git(repo_path, ["merge-base", "--is-ancestor", "HEAD", "origin/#{branch}"]) do
                {_, 0} ->
                  if not config.dry_run do
                    case git(repo_path, ["pull", "--ff-only"]) do
                      {_, 0} -> %{result | pulled: true}
                      _ -> result
                    end
                  else
                    result
                  end
                _ ->
                  %{result | diverged: true}
              end
            else
              result
            end

            # Push if ahead and owned
            if ahead > 0 and is_pushable?(repo_path) and not config.dry_run do
              case git(repo_path, ["push", "origin", "HEAD"]) do
                {_, 0} -> %{result | pushed: true}
                _ -> result
              end
            else
              result
            end

          _ -> result
        end
      else
        result
      end

    # Auto-commit if requested
    result =
      if config.commit_msg && dirty && not config.dry_run do
        {_, _} = git(repo_path, ["add", "-A"])
        case git(repo_path, ["commit", "-m", config.commit_msg]) do
          {_, 0} ->
            if config.auto_push && is_pushable?(repo_path) do
              case git(repo_path, ["push", "origin", "HEAD"]) do
                {_, 0} -> %{result | pushed: true, dirty: false}
                _ -> %{result | dirty: false}
              end
            else
              %{result | dirty: false}
            end
          _ -> result
        end
      else
        result
      end

    # Progress counter
    :counters.add(counter, 1, 1)
    current = :counters.get(counter, 1)
    pct = div(current * 100, total)
    IO.write("\r\e[36m[#{String.pad_leading("#{current}", 3)}/#{total} #{String.pad_leading("#{pct}", 3)}%]\e[0m #{String.pad_trailing(repo, 55)}")

    result
  end

  defp repo_is_dirty?(repo_path) do
    case git(repo_path, ["status", "--porcelain", "--ignore-submodules"]) do
      {output, 0} -> String.trim(output) != ""
      _ -> false
    end
  end

  defp is_pushable?(repo_path) do
    case git(repo_path, ["remote", "get-url", "origin"]) do
      {url, 0} ->
        url = String.trim(url)
        Enum.any?(@owned_prefixes, fn prefix -> String.contains?(url, prefix) end)
      _ -> false
    end
  end

  defp git(repo_path, args) do
    System.cmd("git", ["-C", repo_path | args], stderr_to_stdout: true)
  rescue
    _ -> {"error", 1}
  end

  # --- Phase 1 Summary ---

  defp phase1_summary(results, _config) do
    clean = Enum.count(results, fn r -> not r.dirty end)
    fetched = Enum.count(results, fn r -> r.fetched end)
    pulled = Enum.count(results, fn r -> r.pulled end)
    pushed = Enum.count(results, fn r -> r.pushed end)
    diverged = Enum.count(results, fn r -> r.diverged end)

    IO.puts("\n\e[1m=== Phase 1 Summary ===\e[0m")
    IO.puts("  Fetched:  \e[32m#{fetched}\e[0m")
    IO.puts("  Pulled:   \e[32m#{pulled}\e[0m")
    IO.puts("  Pushed:   \e[32m#{pushed}\e[0m")
    IO.puts("  Clean:    \e[32m#{clean}\e[0m")
    if diverged > 0, do: IO.puts("  Diverged: \e[33m#{diverged}\e[0m")
  end

  # --- Phase 2: Uncommitted (interactive) ---

  defp phase2_uncommitted([], _config) do
    IO.puts("\n\e[1m=== Phase 2: Uncommitted Changes ===\e[0m")
    IO.puts("\e[32mNo uncommitted changes!\e[0m")
  end

  defp phase2_uncommitted(dirty_results, config) do
    IO.puts("\n\e[1m=== Phase 2: Uncommitted Changes (#{length(dirty_results)} repos) ===\e[0m")

    if config.auto_mode do
      IO.puts("\e[2m(Auto mode: skipping #{length(dirty_results)} dirty repos)\e[0m")
      for r <- dirty_results do
        IO.puts("  \e[33m#{r.repo}\e[0m")
      end
    else
      for r <- dirty_results do
        repo_path = Path.join(config.repos_dir, r.repo)
        {status, _} = git(repo_path, ["status", "-s", "--ignore-submodules"])
        lines = status |> String.split("\n", trim: true)
        IO.puts("\n\e[1m--- #{r.repo} ---\e[0m")
        IO.puts(status)
        IO.puts("\e[2m(#{length(lines)} file(s))\e[0m")

        action = prompt("Action? [c]ommit / [d]iscard / [i]gnore: ", "i")
        case action do
          "c" ->
            msg = prompt("Commit message: ", "chore: sync changes")
            {_, _} = git(repo_path, ["add", "-A"])
            git(repo_path, ["commit", "-m", msg])
            if is_pushable?(repo_path) and not config.dry_run do
              git(repo_path, ["push", "origin", "HEAD"])
            end
          "d" ->
            confirm = prompt("Discard ALL changes in #{r.repo}? [y/n]: ", "n")
            if confirm == "y" and not config.dry_run do
              git(repo_path, ["checkout", "--", "."])
              git(repo_path, ["clean", "-fd"])
              IO.puts("\e[32mDiscarded.\e[0m")
            end
          _ ->
            IO.puts("Skipping.")
        end
      end
    end
  end

  # --- Phase 3: Diverged (interactive) ---

  defp phase3_diverged([], _config) do
    IO.puts("\n\e[1m=== Phase 3: Diverged Branches ===\e[0m")
    IO.puts("\e[32mNo diverged branches!\e[0m")
  end

  defp phase3_diverged(diverged_results, config) do
    IO.puts("\n\e[1m=== Phase 3: Diverged Branches (#{length(diverged_results)} repos) ===\e[0m")

    if config.auto_mode do
      IO.puts("\e[2m(Auto mode: skipping #{length(diverged_results)} diverged repos)\e[0m")
      for r <- diverged_results, do: IO.puts("  \e[33m#{r.repo}\e[0m")
    else
      for r <- diverged_results do
        repo_path = Path.join(config.repos_dir, r.repo)
        IO.puts("\n\e[1m--- #{r.repo} ---\e[0m")

        action = prompt("Action? [m]erge / [r]ebase / [i]gnore: ", "i")
        case action do
          "m" -> if not config.dry_run, do: git(repo_path, ["pull", "--no-rebase"])
          "r" -> if not config.dry_run, do: git(repo_path, ["pull", "--rebase"])
          _ -> IO.puts("Skipping.")
        end
      end
    end
  end

  # --- Helpers ---

  defp prompt(msg, default) do
    case IO.gets(msg) do
      :eof -> default
      data -> data |> String.trim() |> then(fn s -> if s == "", do: default, else: s end)
    end
  end

  defp write_report(path, results, repos, elapsed_s) do
    clean = Enum.count(results, fn r -> not r.dirty end)
    fetched = Enum.count(results, fn r -> r.fetched end)
    pulled = Enum.count(results, fn r -> r.pulled end)
    pushed = Enum.count(results, fn r -> r.pushed end)
    dirty_repos = results |> Enum.filter(fn r -> r.dirty end) |> Enum.map(fn r -> r.repo end)

    content = """
    --- sync-all-parallel report #{DateTime.utc_now() |> DateTime.to_iso8601()} ---
    Repos: #{length(repos)}
    Clean: #{clean}, Fetched: #{fetched}, Pulled: #{pulled}, Pushed: #{pushed}
    Elapsed: #{div(elapsed_s, 60)}m#{rem(elapsed_s, 60)}s
    Dirty repos: #{Enum.join(dirty_repos, ", ")}
    """

    File.write!(path, content)
  end
end

SyncAll.main(System.argv())
