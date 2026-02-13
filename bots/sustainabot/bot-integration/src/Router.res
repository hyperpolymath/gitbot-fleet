// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
//
// HTTP Router for Oikos Bot
// Adapted from hyperpolymath/rescript-wasm-runtime

type method = GET | POST | PUT | DELETE | PATCH | OPTIONS | HEAD

type handler = Deno.Request.t => promise<Deno.Response.t>

type middleware = (Deno.Request.t, unit => promise<Deno.Response.t>) => promise<Deno.Response.t>

type route = {
  method: method,
  path: string,
  handler: handler,
}

type t = {
  routes: array<route>,
  middlewares: array<middleware>,
  notFoundHandler: option<handler>,
}

let make = (): t => {
  routes: [],
  middlewares: [],
  notFoundHandler: None,
}

let methodToString = (method: method): string => {
  switch method {
  | GET => "GET"
  | POST => "POST"
  | PUT => "PUT"
  | DELETE => "DELETE"
  | PATCH => "PATCH"
  | OPTIONS => "OPTIONS"
  | HEAD => "HEAD"
  }
}

let methodFromString = (str: string): option<method> => {
  switch str {
  | "GET" => Some(GET)
  | "POST" => Some(POST)
  | "PUT" => Some(PUT)
  | "DELETE" => Some(DELETE)
  | "PATCH" => Some(PATCH)
  | "OPTIONS" => Some(OPTIONS)
  | "HEAD" => Some(HEAD)
  | _ => None
  }
}

// Path matching with named parameters
let matchPath = (pattern: string, path: string): option<Js.Dict.t<string>> => {
  let patternParts = pattern->Js.String2.split("/")->Js.Array2.filter(p => p !== "")
  let pathParts = path->Js.String2.split("/")->Js.Array2.filter(p => p !== "")

  if Js.Array2.length(patternParts) !== Js.Array2.length(pathParts) {
    None
  } else {
    let params = Js.Dict.empty()
    let matches = ref(true)

    patternParts->Js.Array2.forEachi((patternPart, i) => {
      let pathPart = pathParts->Js.Array2.unsafe_get(i)

      if Js.String2.startsWith(patternPart, ":") {
        let paramName = Js.String2.sliceToEnd(patternPart, ~from=1)
        Js.Dict.set(params, paramName, pathPart)
      } else if patternPart !== pathPart {
        matches := false
      }
    })

    if matches.contents {
      Some(params)
    } else {
      None
    }
  }
}

// Add route
let addRoute = (router: t, method: method, path: string, handler: handler): t => {
  let newRoute = {method, path, handler}
  {...router, routes: router.routes->Js.Array2.concat([newRoute])}
}

// Route registration helpers
let get = (router: t, path: string, handler: handler): t => {
  addRoute(router, GET, path, handler)
}

let post = (router: t, path: string, handler: handler): t => {
  addRoute(router, POST, path, handler)
}

let put = (router: t, path: string, handler: handler): t => {
  addRoute(router, PUT, path, handler)
}

let delete = (router: t, path: string, handler: handler): t => {
  addRoute(router, DELETE, path, handler)
}

let patch = (router: t, path: string, handler: handler): t => {
  addRoute(router, PATCH, path, handler)
}

let options = (router: t, path: string, handler: handler): t => {
  addRoute(router, OPTIONS, path, handler)
}

// Add middleware
let use = (router: t, middleware: middleware): t => {
  {...router, middlewares: router.middlewares->Js.Array2.concat([middleware])}
}

// Set custom 404 handler
let notFound = (router: t, handler: handler): t => {
  {...router, notFoundHandler: Some(handler)}
}

// URL parsing helper
module Url = {
  type t
  @new external make: string => t = "URL"
  @get external pathname: t => string = "pathname"
}

// Handle request
let handle = async (router: t, req: Deno.Request.t): Deno.Response.t => {
  let methodStr = Deno.Request.method_(req)
  let url = Deno.Request.url(req)
  let urlObj = Url.make(url)
  let path = Url.pathname(urlObj)

  let methodEnum = methodFromString(methodStr)

  switch methodEnum {
  | None => Deno.Response.make("Method not allowed", {"status": 405})
  | Some(m) => {
      // Find matching route
      let matchingRoute = ref(None)

      router.routes->Js.Array2.forEach(route => {
        if route.method === m {
          switch matchPath(route.path, path) {
          | Some(_params) => matchingRoute := Some(route)
          | None => ()
          }
        }
      })

      switch matchingRoute.contents {
      | None => {
          // No route found, use 404 handler
          switch router.notFoundHandler {
          | Some(handler) => await handler(req)
          | None => Deno.Response.make("Not Found", {"status": 404})
          }
        }
      | Some(route) => {
          // Apply middlewares (in reverse order to build the chain)
          let finalHandler = () => route.handler(req)

          let wrappedHandler = router.middlewares->Js.Array2.reduceRight(
            (next, middleware) => {
              () => middleware(req, next)
            },
            finalHandler,
          )

          await wrappedHandler()
        }
      }
    }
  }
}

// Start server with router
let serve = (router: t, ~port: int) => {
  Deno.serve(
    {"port": port},
    async (req) => await handle(router, req),
  )
}
