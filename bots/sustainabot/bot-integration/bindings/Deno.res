// ReScript bindings for Deno runtime APIs

module Env = {
  @val @scope(("Deno", "env")) external get: string => option<string> = "get"
  @val @scope(("Deno", "env")) external set: (string, string) => unit = "set"
}

module Request = {
  type t
  @get external url: t => string = "url"
  @get external method_: t => string = "method"
  @send external headers: t => Js.Dict.t<string> = "headers"
  @send external json: t => promise<Js.Json.t> = "json"
  @send external text: t => promise<string> = "text"
}

module Response = {
  type t
  @new @scope("globalThis")
  external make: (string, {..}) => t = "Response"
  @new @scope("globalThis")
  external makeJson: (string, {..}) => t = "Response"
}

module HttpServer = {
  type t
  @send external shutdown: t => promise<unit> = "shutdown"
  @send external finished: t => promise<unit> = "finished"
}

@val @scope("Deno")
external serve: ({..}, Request.t => promise<Response.t>) => HttpServer.t = "serve"

// Legacy module for backwards compatibility
module Http = {
  type request = Request.t
  type response = Response.t

  type remoteAddr = {hostname: string, port: int}
  type connInfo = {remoteAddr: remoteAddr}

  type handler = (request, connInfo) => promise<response>

  type listenInfo = {hostname: string, port: int}
  type serveOptions = {
    port?: int,
    hostname?: string,
    onListen?: listenInfo => unit,
  }

  @get external url: request => string = "url"
  @get external method_: request => string = "method"
  @get external headers: request => Js.Dict.t<string> = "headers"
  @send external json: request => promise<Js.Json.t> = "json"
  @send external text: request => promise<string> = "text"

  @new @scope("globalThis")
  external makeResponse: (string, {..}) => response = "Response"

  @new @scope("globalThis")
  external makeJsonResponse: (string, {..}) => response = "Response"

  @val @scope("Deno")
  external serve: (serveOptions, handler) => unit = "serve"
}

module Crypto = {
  type subtleCrypto
  type cryptoKey

  @val @scope(("globalThis", "crypto"))
  external subtle: subtleCrypto = "subtle"

  // Import key with Uint8Array (for HMAC)
  @send
  external importKey: (
    subtleCrypto,
    string,
    Js.TypedArray2.Uint8Array.t,
    {..},
    bool,
    array<string>,
  ) => promise<cryptoKey> = "importKey"

  // Import key with JsonWebKey (for RSA)
  @send
  external importKeyJwk: (
    subtleCrypto,
    @as("jwk") _,
    {..},
    {..},
    bool,
    array<string>,
  ) => promise<cryptoKey> = "importKey"

  // Import key with PKCS8 format (for RSA private keys)
  @send
  external importKeyPkcs8: (
    subtleCrypto,
    @as("pkcs8") _,
    Js.TypedArray2.ArrayBuffer.t,
    {..},
    bool,
    array<string>,
  ) => promise<cryptoKey> = "importKey"

  @send
  external sign: (subtleCrypto, string, cryptoKey, Js.TypedArray2.Uint8Array.t) => promise<Js.TypedArray2.ArrayBuffer.t> =
    "sign"

  @send
  external signWithAlgorithm: (subtleCrypto, {..}, cryptoKey, Js.TypedArray2.Uint8Array.t) => promise<Js.TypedArray2.ArrayBuffer.t> =
    "sign"

  @send
  external verify: (
    subtleCrypto,
    string,
    cryptoKey,
    Js.TypedArray2.Uint8Array.t,
    Js.TypedArray2.Uint8Array.t,
  ) => promise<bool> = "verify"
}

module ArrayBuffer = {
  @new @scope("globalThis")
  external makeUint8Array: Js.TypedArray2.ArrayBuffer.t => Js.TypedArray2.Uint8Array.t = "Uint8Array"
}

module Base64 = {
  @val @scope("globalThis") external btoa: string => string = "btoa"
  @val @scope("globalThis") external atob: string => string = "atob"
}

module TextEncoder = {
  type t

  @new @scope("globalThis") external make: unit => t = "TextEncoder"
  @send external encode: (t, string) => Js.TypedArray2.Uint8Array.t = "encode"
}

module TextDecoder = {
  type t

  @new @scope("globalThis") external make: unit => t = "TextDecoder"
  @send external decode: (t, Js.TypedArray2.Uint8Array.t) => string = "decode"
}

module Fetch = {
  type response

  @val @scope("globalThis")
  external fetch: (string, {..}) => promise<response> = "fetch"

  @get external ok: response => bool = "ok"
  @get external status: response => int = "status"
  @get external statusText: response => string = "statusText"
  @send external json: response => promise<Js.Json.t> = "json"
  @send external text: response => promise<string> = "text"
}
