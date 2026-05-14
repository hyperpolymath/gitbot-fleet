// GitHub REST API Client
// Authenticated API calls using installation access tokens

open Deno

let userAgent = "oikos-bot/0.1.0-beta"
let apiVersion = "2022-11-28"

// Make an authenticated request to GitHub API
let apiRequest = async (
  token: string,
  method: string,
  endpoint: string,
  ~body: option<Js.Json.t>=?,
): result<Js.Json.t, string> => {
  let url = `https://api.github.com${endpoint}`

  let headers = {
    "Authorization": `Bearer ${token}`,
    "Accept": "application/vnd.github+json",
    "X-GitHub-Api-Version": apiVersion,
    "User-Agent": userAgent,
    "Content-Type": "application/json",
  }

  try {
    let response = switch body {
    | Some(b) =>
      await Fetch.fetch(
        url,
        {
          "method": method,
          "headers": headers,
          "body": Js.Json.stringify(b),
        },
      )
    | None =>
      await Fetch.fetch(
        url,
        {
          "method": method,
          "headers": headers,
        },
      )
    }

    if !Fetch.ok(response) {
      let status = Fetch.status(response)
      let errorBody = await Fetch.text(response)
      Error(`GitHub API error ${Belt.Int.toString(status)}: ${errorBody}`)
    } else {
      let json = await Fetch.json(response)
      Ok(json)
    }
  } catch {
  | exn =>
    let msg = switch Js.Exn.asJsExn(exn) {
    | Some(jsExn) => Js.Exn.message(jsExn)->Belt.Option.getWithDefault("Unknown error")
    | None => "Unknown error"
    }
    Error(`API request failed: ${msg}`)
  }
}

// Post a comment on a pull request
let postPRComment = async (
  token: string,
  owner: string,
  repo: string,
  prNumber: int,
  body: string,
): result<int, string> => {
  let endpoint = `/repos/${owner}/${repo}/issues/${Belt.Int.toString(prNumber)}/comments`
  let payload = Js.Json.object_(Js.Dict.fromArray([("body", Js.Json.string(body))]))

  let result = await apiRequest(token, "POST", endpoint, ~body=payload)

  switch result {
  | Ok(json) =>
    switch Js.Json.decodeObject(json) {
    | Some(obj) =>
      switch Js.Dict.get(obj, "id") {
      | Some(id) =>
        switch Js.Json.decodeNumber(id) {
        | Some(num) => Ok(Belt.Float.toInt(num))
        | None => Error("Invalid comment ID in response")
        }
      | None => Error("No comment ID in response")
      }
    | None => Error("Invalid JSON response")
    }
  | Error(e) => Error(e)
  }
}

// Update an existing comment
let updateComment = async (
  token: string,
  owner: string,
  repo: string,
  commentId: int,
  body: string,
): result<unit, string> => {
  let endpoint = `/repos/${owner}/${repo}/issues/comments/${Belt.Int.toString(commentId)}`
  let payload = Js.Json.object_(Js.Dict.fromArray([("body", Js.Json.string(body))]))

  let result = await apiRequest(token, "PATCH", endpoint, ~body=payload)

  switch result {
  | Ok(_) => Ok()
  | Error(e) => Error(e)
  }
}

// Create a check run (for CI status reporting)
let createCheckRun = async (
  token: string,
  owner: string,
  repo: string,
  headSha: string,
  name: string,
  conclusion: string, // "success", "failure", "neutral", "cancelled", "skipped", "timed_out", "action_required"
  ~title: string,
  ~summary: string,
): result<int, string> => {
  let endpoint = `/repos/${owner}/${repo}/check-runs`
  let payload = Js.Json.object_(
    Js.Dict.fromArray([
      ("name", Js.Json.string(name)),
      ("head_sha", Js.Json.string(headSha)),
      ("status", Js.Json.string("completed")),
      ("conclusion", Js.Json.string(conclusion)),
      (
        "output",
        Js.Json.object_(
          Js.Dict.fromArray([("title", Js.Json.string(title)), ("summary", Js.Json.string(summary))]),
        ),
      ),
    ]),
  )

  let result = await apiRequest(token, "POST", endpoint, ~body=payload)

  switch result {
  | Ok(json) =>
    switch Js.Json.decodeObject(json) {
    | Some(obj) =>
      switch Js.Dict.get(obj, "id") {
      | Some(id) =>
        switch Js.Json.decodeNumber(id) {
        | Some(num) => Ok(Belt.Float.toInt(num))
        | None => Error("Invalid check run ID in response")
        }
      | None => Error("No check run ID in response")
      }
    | None => Error("Invalid JSON response")
    }
  | Error(e) => Error(e)
  }
}

// Get pull request details
let getPullRequest = async (
  token: string,
  owner: string,
  repo: string,
  prNumber: int,
): result<Js.Json.t, string> => {
  let endpoint = `/repos/${owner}/${repo}/pulls/${Belt.Int.toString(prNumber)}`
  await apiRequest(token, "GET", endpoint)
}

// Get repository details
let getRepository = async (token: string, owner: string, repo: string): result<Js.Json.t, string> => {
  let endpoint = `/repos/${owner}/${repo}`
  await apiRequest(token, "GET", endpoint)
}
