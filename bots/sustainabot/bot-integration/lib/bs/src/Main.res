// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2024-2025 hyperpolymath
//
// Oikos Bot Main Entry Point
// ReScript + Deno implementation

open Deno
open Types

// Logger helper
let log = (level: string, msg: string, data: option<Js.Json.t>) => {
  let timestamp = Js.Date.toISOString(Js.Date.make())
  let logObj = switch data {
  | Some(d) =>
    Js.Dict.fromArray([
      ("timestamp", Js.Json.string(timestamp)),
      ("level", Js.Json.string(level)),
      ("message", Js.Json.string(msg)),
      ("data", d),
    ])
  | None =>
    Js.Dict.fromArray([
      ("timestamp", Js.Json.string(timestamp)),
      ("level", Js.Json.string(level)),
      ("message", Js.Json.string(msg)),
    ])
  }
  Js.Console.log(Js.Json.stringify(Js.Json.object_(logObj)))
}

let info = (msg, ~data=?) => log("info", msg, data)
let error = (msg, ~data=?) => log("error", msg, data)

// Extract PR info from GitHub payload
let extractPRInfo = (payload: Js.Json.t): (int, string, string) => {
  switch Js.Json.decodeObject(payload) {
  | Some(obj) =>
    let prNumber = switch Js.Dict.get(obj, "number") {
    | Some(n) =>
      switch Js.Json.decodeNumber(n) {
      | Some(num) => Belt.Float.toInt(num)
      | None => 0
      }
    | None => 0
    }
    let (baseSha, headSha) = switch Js.Dict.get(obj, "pull_request") {
    | Some(pr) =>
      switch Js.Json.decodeObject(pr) {
      | Some(prObj) =>
        let base = switch Js.Dict.get(prObj, "base") {
        | Some(b) =>
          switch Js.Json.decodeObject(b) {
          | Some(baseObj) =>
            switch Js.Dict.get(baseObj, "sha") {
            | Some(s) => Js.Json.decodeString(s)->Belt.Option.getWithDefault("")
            | None => ""
            }
          | None => ""
          }
        | None => ""
        }
        let head = switch Js.Dict.get(prObj, "head") {
        | Some(h) =>
          switch Js.Json.decodeObject(h) {
          | Some(headObj) =>
            switch Js.Dict.get(headObj, "sha") {
            | Some(s) => Js.Json.decodeString(s)->Belt.Option.getWithDefault("")
            | None => ""
            }
          | None => ""
          }
        | None => ""
        }
        (base, head)
      | None => ("", "")
      }
    | None => ("", "")
    }
    (prNumber, baseSha, headSha)
  | None => (0, "", "")
  }
}

// Extract MR info from GitLab payload
let extractMRInfo = (payload: Js.Json.t): (int, string, string) => {
  switch Js.Json.decodeObject(payload) {
  | Some(obj) =>
    switch Js.Dict.get(obj, "object_attributes") {
    | Some(attrs) =>
      switch Js.Json.decodeObject(attrs) {
      | Some(attrsObj) =>
        let mrIid = switch Js.Dict.get(attrsObj, "iid") {
        | Some(n) =>
          switch Js.Json.decodeNumber(n) {
          | Some(num) => Belt.Float.toInt(num)
          | None => 0
          }
        | None => 0
        }
        let baseSha = switch Js.Dict.get(attrsObj, "diff_refs") {
        | Some(refs) =>
          switch Js.Json.decodeObject(refs) {
          | Some(refsObj) =>
            switch Js.Dict.get(refsObj, "base_sha") {
            | Some(s) => Js.Json.decodeString(s)->Belt.Option.getWithDefault("")
            | None => ""
            }
          | None => ""
          }
        | None => ""
        }
        let headSha = switch Js.Dict.get(attrsObj, "last_commit") {
        | Some(commit) =>
          switch Js.Json.decodeObject(commit) {
          | Some(commitObj) =>
            switch Js.Dict.get(commitObj, "id") {
            | Some(s) => Js.Json.decodeString(s)->Belt.Option.getWithDefault("")
            | None => ""
            }
          | None => ""
          }
        | None => ""
        }
        (mrIid, baseSha, headSha)
      | None => (0, "", "")
      }
    | None => (0, "", "")
    }
  | None => (0, "", "")
  }
}

// JSON response helper
let jsonResponse = (data: Js.Json.t, ~status=200): Http.response => {
  Http.makeJsonResponse(
    Js.Json.stringify(data),
    {
      "status": status,
      "headers": {"Content-Type": "application/json"},
    },
  )
}

// Validate GitHub signature if configured
let validateGitHubSignature = async (
  config: config,
  headers: Js.Dict.t<string>,
  body: string,
): option<Http.response> => {
  switch config.githubWebhookSecret {
  | Some(secret) =>
    let signature = Js.Dict.get(headers, "x-hub-signature-256")->Belt.Option.getWithDefault("")
    let valid = await Webhook.verifyGitHubSignature(body, signature, secret)
    if !valid {
      error("Invalid GitHub webhook signature")
      Some(Http.makeResponse(`{"error": "Invalid signature"}`, {"status": 401}))
    } else {
      None
    }
  | None => None
  }
}

// Handle GitHub webhook
let handleGitHubWebhook = async (
  config: config,
  headers: Js.Dict.t<string>,
  body: string,
): Http.response => {
  // Verify signature if secret is configured
  let signatureError = await validateGitHubSignature(config, headers, body)
  switch signatureError {
  | Some(errResponse) => errResponse
  | None =>
    // Parse payload
    let parseResult = try {
      Some(Js.Json.parseExn(body))
    } catch {
    | _ => None
    }

    switch parseResult {
    | None => Http.makeResponse(`{"error": "Invalid JSON"}`, {"status": 400})
    | Some(payload) =>
      let event = Webhook.parseGitHubEvent(headers, payload)
      switch event {
      | Some(e) =>
        info(
          `GitHub event: ${e.eventType}`,
          ~data=Js.Json.object_(
            Js.Dict.fromArray([
              ("repo", Js.Json.string(`${e.repository.owner}/${e.repository.name}`)),
              ("action", Js.Json.string(e.action->Belt.Option.getWithDefault(""))),
            ]),
          ),
        )

        // Handle pull request events
        if e.eventType == "pull_request" {
          let action = e.action->Belt.Option.getWithDefault("")
          if action == "opened" || action == "synchronize" {
            // Extract PR info from payload
            let (prNumber, baseSha, headSha) = extractPRInfo(payload)

            // Call real analyzer
            let analysisResult = await Analysis.analyzeDiff(
              config.analysisEndpoint,
              e.repository.url,
              baseSha,
              headSha,
            )

            let comment = switch analysisResult {
            | Ok(analysis) =>
              Report.generatePRComment(analysis, config.mode)
            | Error(err) =>
              error(`Analysis failed: ${err}`)
              let analysis = Analysis.mockAnalysis()
              Report.generatePRComment(analysis, config.mode)
            }

            // Post comment to GitHub if authenticated
            let authResult = await GitHubApp.getAuthToken(config, payload)
            switch authResult {
            | Ok(token) =>
              let postResult = await GitHubAPI.postPRComment(
                token,
                e.repository.owner,
                e.repository.name,
                prNumber,
                comment,
              )
              switch postResult {
              | Ok(commentId) =>
                info(
                  `Posted PR comment`,
                  ~data=Js.Json.object_(
                    Js.Dict.fromArray([
                      ("pr", Js.Json.number(Belt.Int.toFloat(prNumber))),
                      ("commentId", Js.Json.number(Belt.Int.toFloat(commentId))),
                    ]),
                  ),
                )
              | Error(err) =>
                error(`Failed to post PR comment: ${err}`)
              }
            | Error(err) =>
              // Not configured for GitHub App - log comment instead
              info(
                `GitHub App not configured, comment not posted: ${err}`,
                ~data=Js.Json.string(comment),
              )
            }
          }
        }

        jsonResponse(Js.Json.object_(Js.Dict.fromArray([("status", Js.Json.string("processed"))])))
      | None =>
        error("Failed to parse GitHub event")
        Http.makeResponse(`{"error": "Invalid event"}`, {"status": 400})
      }
    }
  }
}

// Validate GitLab token if configured
let validateGitLabToken = (
  config: config,
  headers: Js.Dict.t<string>,
): option<Http.response> => {
  switch config.gitlabWebhookSecret {
  | Some(secret) =>
    let token = Js.Dict.get(headers, "x-gitlab-token")->Belt.Option.getWithDefault("")
    if !Webhook.verifyGitLabToken(token, secret) {
      error("Invalid GitLab webhook token")
      Some(Http.makeResponse(`{"error": "Invalid token"}`, {"status": 401}))
    } else {
      None
    }
  | None => None
  }
}

// Handle GitLab webhook
let handleGitLabWebhook = async (
  config: config,
  headers: Js.Dict.t<string>,
  body: string,
): Http.response => {
  // Verify token if secret is configured
  let tokenError = validateGitLabToken(config, headers)
  switch tokenError {
  | Some(errResponse) => errResponse
  | None =>
    // Parse payload
    let parseResult = try {
      Some(Js.Json.parseExn(body))
    } catch {
    | _ => None
    }

    switch parseResult {
    | None => Http.makeResponse(`{"error": "Invalid JSON"}`, {"status": 400})
    | Some(payload) =>
      let event = Webhook.parseGitLabEvent(headers, payload)
      switch event {
      | Some(e) =>
        info(
          `GitLab event: ${e.eventType}`,
          ~data=Js.Json.object_(
            Js.Dict.fromArray([("repo", Js.Json.string(`${e.repository.owner}/${e.repository.name}`))]),
          ),
        )

        // Handle merge request events
        if e.eventType == "Merge Request Hook" {
          let (mrIid, baseSha, headSha) = extractMRInfo(payload)

          let analysisResult = await Analysis.analyzeDiff(
            config.analysisEndpoint,
            e.repository.url,
            baseSha,
            headSha,
          )

          switch analysisResult {
          | Ok(analysis) =>
            let comment = Report.generatePRComment(analysis, config.mode)
            info(`Generated MR comment for MR !${Belt.Int.toString(mrIid)}`, ~data=Js.Json.string(comment))
          | Error(err) =>
            error(`Analysis failed: ${err}`)
            let analysis = Analysis.mockAnalysis()
            let comment = Report.generatePRComment(analysis, config.mode)
            info(`Generated fallback MR comment`, ~data=Js.Json.string(comment))
          }
        }

        jsonResponse(Js.Json.object_(Js.Dict.fromArray([("status", Js.Json.string("processed"))])))
      | None =>
        error("Failed to parse GitLab event")
        Http.makeResponse(`{"error": "Invalid event"}`, {"status": 400})
      }
    }
  }
}

// Main request handler
let handler = (config: config): Http.handler => {
  async (req, _connInfo) => {
    let url = Http.url(req)
    let method = Http.method_(req)
    let path = Js.String2.replaceByRe(url, %re("/^https?:\/\/[^\/]+/"), "")

    // Route requests
    switch (method, path) {
    | ("GET", "/health") =>
      jsonResponse(
        Js.Json.object_(
          Js.Dict.fromArray([
            ("status", Js.Json.string("healthy")),
            ("mode", Js.Json.string(Config.modeToString(config.mode))),
          ]),
        ),
      )

    | ("POST", "/webhooks/github") =>
      let body = await Http.text(req)
      let headers = Http.headers(req)
      await handleGitHubWebhook(config, headers, body)

    | ("POST", "/webhooks/gitlab") =>
      let body = await Http.text(req)
      let headers = Http.headers(req)
      await handleGitLabWebhook(config, headers, body)

    | ("GET", "/metrics") =>
      // OpenTelemetry metrics endpoint
      jsonResponse(
        Js.Json.object_(
          Js.Dict.fromArray([
            ("oikos_bot_requests_total", Js.Json.number(0.0)),
            ("oikos_bot_analyses_total", Js.Json.number(0.0)),
          ]),
        ),
      )

    | _ => Http.makeResponse("Not Found", {"status": 404})
    }
  }
}

// Main entry point
let main = () => {
  switch Config.load() {
  | Ok(config) =>
    info(
      `Starting Oikos Bot`,
      ~data=Js.Json.object_(
        Js.Dict.fromArray([
          ("port", Js.Json.number(Belt.Int.toFloat(config.port))),
          ("mode", Js.Json.string(Config.modeToString(config.mode))),
        ]),
      ),
    )

    Http.serve(
      {
        port: config.port,
        onListen: ({hostname, port}) => {
          info(`Server listening on ${hostname}:${Belt.Int.toString(port)}`)
        },
      },
      handler(config),
    )

  | Error(e) =>
    error(`Failed to load config: ${e}`)
  }
}

// Run
main()
