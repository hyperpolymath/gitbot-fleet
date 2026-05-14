// Webhook signature verification and parsing

open Deno

let verifyGitHubSignature = async (
  payload: string,
  signature: string,
  secret: string,
): bool => {
  // GitHub uses HMAC-SHA256
  let encoder = TextEncoder.make()
  let keyData = encoder->TextEncoder.encode(secret)
  let data = encoder->TextEncoder.encode(payload)

  let key = await Crypto.subtle->Crypto.importKey(
    "raw",
    keyData,
    {"name": "HMAC", "hash": "SHA-256"},
    false,
    ["sign", "verify"],
  )

  let signatureBytes = encoder->TextEncoder.encode(
    Js.String.replace("sha256=", "", signature),
  )

  await Crypto.subtle->Crypto.verify("HMAC", key, signatureBytes, data)
}

let verifyGitLabToken = (token: string, secret: string): bool => {
  token == secret
}

let parseGitHubEvent = (
  headers: Js.Dict.t<string>,
  payload: Js.Json.t,
): option<Types.webhookEvent> => {
  let eventType = Js.Dict.get(headers, "x-github-event")
  let action = switch Js.Json.decodeObject(payload) {
  | Some(obj) =>
    switch Js.Dict.get(obj, "action") {
    | Some(a) => Js.Json.decodeString(a)
    | None => None
    }
  | None => None
  }

  switch eventType {
  | Some(et) =>
    // Extract repository info
    let repo = switch Js.Json.decodeObject(payload) {
    | Some(obj) =>
      switch Js.Dict.get(obj, "repository") {
      | Some(r) =>
        switch Js.Json.decodeObject(r) {
        | Some(repoObj) => {
            let owner = switch Js.Dict.get(repoObj, "owner") {
            | Some(o) =>
              switch Js.Json.decodeObject(o) {
              | Some(ownerObj) =>
                switch Js.Dict.get(ownerObj, "login") {
                | Some(l) => Js.Json.decodeString(l)->Belt.Option.getWithDefault("")
                | None => ""
                }
              | None => ""
              }
            | None => ""
            }
            let name = switch Js.Dict.get(repoObj, "name") {
            | Some(n) => Js.Json.decodeString(n)->Belt.Option.getWithDefault("")
            | None => ""
            }
            let url = switch Js.Dict.get(repoObj, "html_url") {
            | Some(u) => Js.Json.decodeString(u)->Belt.Option.getWithDefault("")
            | None => ""
            }
            Some({Types.owner, name, url})
          }
        | None => None
        }
      | None => None
      }
    | None => None
    }

    switch repo {
    | Some(r) =>
      Some({
        Types.platform: Types.GitHub,
        eventType: et,
        action,
        repository: r,
        payload,
      })
    | None => None
    }
  | None => None
  }
}

let parseGitLabEvent = (
  headers: Js.Dict.t<string>,
  payload: Js.Json.t,
): option<Types.webhookEvent> => {
  let eventType = Js.Dict.get(headers, "x-gitlab-event")

  switch eventType {
  | Some(et) =>
    // Extract project info
    let repo = switch Js.Json.decodeObject(payload) {
    | Some(obj) =>
      switch Js.Dict.get(obj, "project") {
      | Some(p) =>
        switch Js.Json.decodeObject(p) {
        | Some(projObj) => {
            let namespace = switch Js.Dict.get(projObj, "namespace") {
            | Some(n) => Js.Json.decodeString(n)->Belt.Option.getWithDefault("")
            | None => ""
            }
            let name = switch Js.Dict.get(projObj, "name") {
            | Some(n) => Js.Json.decodeString(n)->Belt.Option.getWithDefault("")
            | None => ""
            }
            let url = switch Js.Dict.get(projObj, "web_url") {
            | Some(u) => Js.Json.decodeString(u)->Belt.Option.getWithDefault("")
            | None => ""
            }
            Some({Types.owner: namespace, name, url})
          }
        | None => None
        }
      | None => None
      }
    | None => None
    }

    switch repo {
    | Some(r) =>
      Some({
        Types.platform: Types.GitLab,
        eventType: et,
        action: None,
        repository: r,
        payload,
      })
    | None => None
    }
  | None => None
  }
}
