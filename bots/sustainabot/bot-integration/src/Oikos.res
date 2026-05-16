// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
//
// Oikos - Ecological & Economic Code Analysis
// οἶκος: Greek root of both "ecology" and "economy"
// TEA Architecture - Model-Update-Subscriptions pattern

open ServerTea

// =============================================================================
// MODEL
// =============================================================================

type botMode =
  | Advisor
  | Consultant
  | Regulator

type webhookSource =
  | GitHub
  | GitLab

type analysisStatus =
  | Pending
  | InProgress
  | Completed(Types.analysisResult)
  | Failed(string)

type pendingAnalysis = {
  id: string,
  repo: string,
  prNumber: int,
  status: analysisStatus,
  createdAt: float,
}

type model = {
  mode: botMode,
  port: int,
  webhookSecret: option<string>,
  appId: option<string>,
  privateKeyPath: option<string>,
  pendingAnalyses: array<pendingAnalysis>,
  totalProcessed: int,
  startTime: float,
  healthy: bool,
}

// =============================================================================
// MESSAGES
// =============================================================================

type msg =
  // Webhook events
  | WebhookReceived(webhookSource, Js.Json.t)
  | WebhookVerified(webhookSource, Js.Json.t)
  | WebhookRejected(string)
  // Analysis lifecycle
  | AnalysisRequested(string, string, int) // id, repo, prNumber
  | AnalysisStarted(string)
  | AnalysisCompleted(string, Types.analysisResult)
  | AnalysisFailed(string, string)
  // GitHub API responses
  | CommentPosted(string, int)
  | CommentFailed(string, string)
  // System
  | HealthCheck
  | Tick
  | Shutdown

// =============================================================================
// INIT
// =============================================================================

type flags = {
  port: int,
  mode: string,
  webhookSecret: option<string>,
  appId: option<string>,
  privateKeyPath: option<string>,
}

let modeFromString = str =>
  switch str {
  | "consultant" => Consultant
  | "regulator" => Regulator
  | _ => Advisor
  }

let init = (flags: flags) => {
  let model = {
    mode: modeFromString(flags.mode),
    port: flags.port,
    webhookSecret: flags.webhookSecret,
    appId: flags.appId,
    privateKeyPath: flags.privateKeyPath,
    pendingAnalyses: [],
    totalProcessed: 0,
    startTime: Js.Date.now(),
    healthy: true,
  }

  Js.Console.log(`🏛️ Oikos Bot starting...`)
  Js.Console.log(`   Mode: ${flags.mode}`)
  Js.Console.log(`   Port: ${flags.port->Belt.Int.toString}`)

  (model, Cmd.none)
}

// =============================================================================
// UPDATE
// =============================================================================

let update = (msg: msg, model: model) => {
  switch msg {
  | WebhookReceived(source, payload) => {
      let sourceStr = switch source {
      | GitHub => "GitHub"
      | GitLab => "GitLab"
      }
      Js.Console.log(`📨 Webhook received from ${sourceStr}`)
      // TODO: Verify signature then dispatch WebhookVerified
      (model, Cmd.perform(async () => payload, p => WebhookVerified(source, p)))
    }

  | WebhookVerified(source, payload) => {
      Js.Console.log(`✓ Webhook verified`)
      // Parse the webhook and determine action
      let _ = source
      let _ = payload
      // TODO: Parse event type, extract PR info, start analysis
      ({...model, totalProcessed: model.totalProcessed + 1}, Cmd.none)
    }

  | WebhookRejected(reason) => {
      Js.Console.error(`✗ Webhook rejected: ${reason}`)
      (model, Cmd.none)
    }

  | AnalysisRequested(id, repo, prNumber) => {
      Js.Console.log(`🔍 Analysis requested: ${repo}#${prNumber->Belt.Int.toString}`)
      let analysis = {
        id,
        repo,
        prNumber,
        status: Pending,
        createdAt: Js.Date.now(),
      }
      (
        {...model, pendingAnalyses: model.pendingAnalyses->Js.Array2.concat([analysis])},
        Cmd.none,
      )
    }

  | AnalysisStarted(id) => {
      let pendingAnalyses =
        model.pendingAnalyses->Js.Array2.map(a =>
          if a.id == id {
            {...a, status: InProgress}
          } else {
            a
          }
        )
      ({...model, pendingAnalyses}, Cmd.none)
    }

  | AnalysisCompleted(id, result) => {
      Js.Console.log(`✓ Analysis completed: ${id}`)
      let pendingAnalyses =
        model.pendingAnalyses->Js.Array2.map(a =>
          if a.id == id {
            {...a, status: Completed(result)}
          } else {
            a
          }
        )
      ({...model, pendingAnalyses}, Cmd.none)
    }

  | AnalysisFailed(id, error) => {
      Js.Console.error(`✗ Analysis failed: ${id} - ${error}`)
      let pendingAnalyses =
        model.pendingAnalyses->Js.Array2.map(a =>
          if a.id == id {
            {...a, status: Failed(error)}
          } else {
            a
          }
        )
      ({...model, pendingAnalyses}, Cmd.none)
    }

  | CommentPosted(repo, prNumber) => {
      Js.Console.log(`💬 Comment posted to ${repo}#${prNumber->Belt.Int.toString}`)
      (model, Cmd.none)
    }

  | CommentFailed(repo, error) => {
      Js.Console.error(`✗ Failed to post comment to ${repo}: ${error}`)
      (model, Cmd.none)
    }

  | HealthCheck => {
      Js.Console.log(`💚 Health check - processed: ${model.totalProcessed->Belt.Int.toString}`)
      (model, Cmd.none)
    }

  | Tick => {
      // Periodic cleanup of old analyses
      let now = Js.Date.now()
      let oneHour = 60.0 *. 60.0 *. 1000.0
      let pendingAnalyses =
        model.pendingAnalyses->Js.Array2.filter(a => now -. a.createdAt < oneHour)
      ({...model, pendingAnalyses}, Cmd.none)
    }

  | Shutdown => {
      Js.Console.log(`👋 Shutting down...`)
      ({...model, healthy: false}, Cmd.none)
    }
  }
}

// =============================================================================
// SUBSCRIPTIONS
// =============================================================================

let subscriptions = (model: model) => {
  if model.healthy {
    Sub.batch([
      Sub.httpServer(model.port, json => Some(WebhookReceived(GitHub, json))),
      Sub.every(60000, () => Tick),
    ])
  } else {
    Sub.none
  }
}

// =============================================================================
// RUN
// =============================================================================

let run = () => {
  let port = switch Deno.Env.get("PORT") {
  | Some(p) => Belt.Int.fromString(p)->Belt.Option.getWithDefault(3000)
  | None => 3000
  }
  let mode = Deno.Env.get("BOT_MODE")->Belt.Option.getWithDefault("advisor")
  let webhookSecret = Deno.Env.get("GITHUB_WEBHOOK_SECRET")
  let appId = Deno.Env.get("GITHUB_APP_ID")
  let privateKeyPath = Deno.Env.get("GITHUB_PRIVATE_KEY_PATH")

  let flags = {
    port,
    mode,
    webhookSecret,
    appId,
    privateKeyPath,
  }

  Runtime.make(~init, ~update, ~subscriptions, ~flags)
}

// Auto-run when imported as main module
let _ = run()
