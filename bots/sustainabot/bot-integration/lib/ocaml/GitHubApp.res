// GitHub App JWT Authentication
// Generates RS256 JWTs for GitHub App authentication and manages installation tokens

open Deno

// Token cache - stores installation tokens to avoid unnecessary API calls
let tokenCache: Js.Dict.t<Types.installationToken> = Js.Dict.empty()

// Convert PEM private key to ArrayBuffer for Web Crypto API
let pemToArrayBuffer = (pem: string): Js.TypedArray2.ArrayBuffer.t => {
  // Remove PEM header/footer and whitespace
  let pemContents = pem
    ->Js.String2.replaceByRe(%re("/-----BEGIN (?:RSA )?PRIVATE KEY-----/g"), "")
    ->Js.String2.replaceByRe(%re("/-----END (?:RSA )?PRIVATE KEY-----/g"), "")
    ->Js.String2.replaceByRe(%re("/\\s/g"), "")

  // Decode base64 to binary string
  let binaryStr = Base64.atob(pemContents)
  let len = Js.String.length(binaryStr)

  // Convert to Uint8Array
  let bytes = Js.TypedArray2.Uint8Array.fromLength(len)
  for i in 0 to len - 1 {
    let charCode = Js.String2.charCodeAt(binaryStr, i)->Belt.Float.toInt
    Js.TypedArray2.Uint8Array.unsafe_set(bytes, i, charCode)
  }

  Js.TypedArray2.Uint8Array.buffer(bytes)
}

// Base64URL encode (for JWT)
let base64UrlEncode = (data: Js.TypedArray2.Uint8Array.t): string => {
  // Convert Uint8Array to binary string
  let len = Js.TypedArray2.Uint8Array.length(data)
  let binaryStr = ref("")
  for i in 0 to len - 1 {
    let byte = Js.TypedArray2.Uint8Array.unsafe_get(data, i)
    binaryStr := binaryStr.contents ++ Js.String.fromCharCode(byte)
  }

  // Base64 encode then make URL-safe
  Base64.btoa(binaryStr.contents)
    ->Js.String2.replaceByRe(%re("/\+/g"), "-")
    ->Js.String2.replaceByRe(%re("/\//g"), "_")
    ->Js.String2.replaceByRe(%re("/=+$/g"), "")
}

// Base64URL encode a string
let base64UrlEncodeString = (str: string): string => {
  Base64.btoa(str)
    ->Js.String2.replaceByRe(%re("/\+/g"), "-")
    ->Js.String2.replaceByRe(%re("/\//g"), "_")
    ->Js.String2.replaceByRe(%re("/=+$/g"), "")
}

// Generate JWT for GitHub App authentication
// JWT is valid for 10 minutes (GitHub requirement)
let generateJWT = async (appId: string, privateKeyPem: string): result<string, string> => {
  let nowSeconds = Js.Date.now() /. 1000.0

  // JWT Header
  let header = Js.Dict.fromArray([
    ("alg", Js.Json.string("RS256")),
    ("typ", Js.Json.string("JWT")),
  ])
  let headerB64 = base64UrlEncodeString(Js.Json.stringify(Js.Json.object_(header)))

  // JWT Payload - issued 60 seconds ago to account for clock drift
  let payload = Js.Dict.fromArray([
    ("iss", Js.Json.string(appId)),
    ("iat", Js.Json.number(nowSeconds -. 60.0)),
    ("exp", Js.Json.number(nowSeconds +. 600.0)), // 10 minutes
  ])
  let payloadB64 = base64UrlEncodeString(Js.Json.stringify(Js.Json.object_(payload)))

  // Message to sign
  let message = `${headerB64}.${payloadB64}`

  try {
    // Import RSA private key
    let keyBuffer = pemToArrayBuffer(privateKeyPem)
    let encoder = TextEncoder.make()

    let key = await Crypto.importKeyPkcs8(
      Crypto.subtle,
      keyBuffer,
      {"name": "RSASSA-PKCS1-v1_5", "hash": "SHA-256"},
      false,
      ["sign"],
    )

    // Sign the message
    let messageBytes = TextEncoder.encode(encoder, message)
    let signatureBuffer = await Crypto.signWithAlgorithm(
      Crypto.subtle,
      {"name": "RSASSA-PKCS1-v1_5"},
      key,
      messageBytes,
    )

    // Convert signature to base64url
    let signatureBytes = ArrayBuffer.makeUint8Array(signatureBuffer)
    let signatureB64 = base64UrlEncode(signatureBytes)

    Ok(`${message}.${signatureB64}`)
  } catch {
  | exn =>
    let msg = switch Js.Exn.asJsExn(exn) {
    | Some(jsExn) => Js.Exn.message(jsExn)->Belt.Option.getWithDefault("Unknown error")
    | None => "Unknown error"
    }
    Error(`Failed to generate JWT: ${msg}`)
  }
}

// Get installation access token from GitHub
// Tokens are valid for 1 hour
let getInstallationToken = async (jwt: string, installationId: int): result<Types.installationToken, string> => {
  let cacheKey = Belt.Int.toString(installationId)

  // Check cache first
  switch Js.Dict.get(tokenCache, cacheKey) {
  | Some(cached) if cached.expiresAt > Js.Date.now() +. 60000.0 =>
    // Return cached token if it has at least 1 minute remaining
    Ok(cached)
  | _ =>
    // Fetch new token
    try {
      let response = await Fetch.fetch(
        `https://api.github.com/app/installations/${cacheKey}/access_tokens`,
        {
          "method": "POST",
          "headers": {
            "Authorization": `Bearer ${jwt}`,
            "Accept": "application/vnd.github+json",
            "X-GitHub-Api-Version": "2022-11-28",
            "User-Agent": "oikos-bot",
          },
        },
      )

      if !Fetch.ok(response) {
        let status = Fetch.status(response)
        let body = await Fetch.text(response)
        Error(`GitHub API error ${Belt.Int.toString(status)}: ${body}`)
      } else {
        let json = await Fetch.json(response)
        switch Js.Json.decodeObject(json) {
        | Some(obj) =>
          let token = switch Js.Dict.get(obj, "token") {
          | Some(t) => Js.Json.decodeString(t)->Belt.Option.getWithDefault("")
          | None => ""
          }
          let expiresAtStr = switch Js.Dict.get(obj, "expires_at") {
          | Some(e) => Js.Json.decodeString(e)->Belt.Option.getWithDefault("")
          | None => ""
          }

          if token == "" {
            Error("No token in response")
          } else {
            // Parse ISO 8601 date to timestamp (getTime returns ms since epoch)
            let expiresAtDate = Js.Date.fromString(expiresAtStr)
            let expiresAt = Js.Date.getTime(expiresAtDate)
            let installToken: Types.installationToken = {
              token,
              expiresAt,
            }

            // Cache the token
            Js.Dict.set(tokenCache, cacheKey, installToken)

            Ok(installToken)
          }
        | None => Error("Invalid JSON response")
        }
      }
    } catch {
    | exn =>
      let msg = switch Js.Exn.asJsExn(exn) {
      | Some(jsExn) => Js.Exn.message(jsExn)->Belt.Option.getWithDefault("Unknown error")
      | None => "Unknown error"
      }
      Error(`Failed to get installation token: ${msg}`)
    }
  }
}

// Extract installation ID from webhook payload
let extractInstallationId = (payload: Js.Json.t): option<int> => {
  switch Js.Json.decodeObject(payload) {
  | Some(obj) =>
    switch Js.Dict.get(obj, "installation") {
    | Some(inst) =>
      switch Js.Json.decodeObject(inst) {
      | Some(instObj) =>
        switch Js.Dict.get(instObj, "id") {
        | Some(id) =>
          switch Js.Json.decodeNumber(id) {
          | Some(num) => Some(Belt.Float.toInt(num))
          | None => None
          }
        | None => None
        }
      | None => None
      }
    | None => None
    }
  | None => None
  }
}

// Get an authenticated token for a repository installation
// This is the main entry point for authentication
let getAuthToken = async (config: Types.config, payload: Js.Json.t): result<string, string> => {
  switch (config.githubAppId, config.githubPrivateKey) {
  | (Some(appId), Some(privateKey)) =>
    switch extractInstallationId(payload) {
    | Some(installationId) =>
      let jwtResult = await generateJWT(appId, privateKey)
      switch jwtResult {
      | Ok(jwt) =>
        let tokenResult = await getInstallationToken(jwt, installationId)
        switch tokenResult {
        | Ok(installToken) => Ok(installToken.token)
        | Error(e) => Error(e)
        }
      | Error(e) => Error(e)
      }
    | None => Error("No installation ID in payload")
    }
  | _ => Error("GitHub App credentials not configured")
  }
}
