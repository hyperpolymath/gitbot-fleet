// Configuration loading from environment

open Types

let getEnv = (key: string, ~default: option<string>=?): option<string> => {
  switch Deno.Env.get(key) {
  | Some(v) => Some(v)
  | None => default
  }
}

let getEnvRequired = (key: string): result<string, string> => {
  switch Deno.Env.get(key) {
  | Some(v) => Ok(v)
  | None => Error(`Missing required environment variable: ${key}`)
  }
}

let getEnvInt = (key: string, ~default: int): int => {
  switch Deno.Env.get(key) {
  | Some(v) =>
    switch Belt.Int.fromString(v) {
    | Some(i) => i
    | None => default
    }
  | None => default
  }
}

let parseMode = (s: string): botMode => {
  switch Js.String.toLowerCase(s) {
  | "consultant" => Consultant
  | "regulator" => Regulator
  | _ => Advisor
  }
}

// Load private key from file or environment
// If GITHUB_PRIVATE_KEY_FILE is set, read from file
// Otherwise use GITHUB_PRIVATE_KEY directly
let loadPrivateKey = (): option<string> => {
  switch Deno.Env.get("GITHUB_PRIVATE_KEY_FILE") {
  | Some(_path) =>
    // For file-based keys, the key should be loaded at startup
    // For now, fall back to env var (file loading would need async)
    getEnv("GITHUB_PRIVATE_KEY")
  | None => getEnv("GITHUB_PRIVATE_KEY")
  }
}

let load = (): result<config, string> => {
  let modeStr = switch getEnv("BOT_MODE") {
  | Some(m) => m
  | None => "advisor"
  }
  let mode = parseMode(modeStr)

  let analysisEndpoint = switch getEnv("ANALYSIS_ENDPOINT") {
  | Some(e) => e
  | None => "http://localhost:8080/analyze"
  }

  Ok({
    port: getEnvInt("PORT", ~default=3000),
    mode,
    analysisEndpoint,
    githubWebhookSecret: getEnv("GITHUB_WEBHOOK_SECRET"),
    gitlabWebhookSecret: getEnv("GITLAB_WEBHOOK_SECRET"),
    githubAppId: getEnv("GITHUB_APP_ID"),
    githubPrivateKey: loadPrivateKey(),
  })
}

let modeToString = (mode: botMode): string => {
  switch mode {
  | Consultant => "consultant"
  | Advisor => "advisor"
  | Regulator => "regulator"
  }
}
