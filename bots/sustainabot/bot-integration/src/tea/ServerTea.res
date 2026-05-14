// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
//
// Server-side TEA (The Elm Architecture) for Deno
// Inspired by hyperpolymath/rescript-tea but for backend services

module Cmd = {
  type rec t<'msg> =
    | None
    | Batch(array<t<'msg>>)
    | Perform(unit => promise<'msg>)
    | PerformWithDispatch((('msg) => unit) => promise<unit>)

  let none = None
  let batch = cmds => Batch(cmds)
  let perform = (task, toMsg) => Perform(async () => toMsg(await task()))
  let attempt = (task, toMsg) => Perform(async () => {
    try {
      toMsg(Ok(await task()))
    } catch {
    | exn => toMsg(Error(exn))
    }
  })
  let withDispatch = fn => PerformWithDispatch(fn)
}

module Sub = {
  type rec t<'msg> =
    | None
    | Batch(array<t<'msg>>)
    | HttpServer(int, Js.Json.t => option<'msg>)
    | Interval(int, unit => 'msg)

  let none = None
  let batch = subs => Batch(subs)
  let httpServer = (port, handler) => HttpServer(port, handler)
  let every = (ms, toMsg) => Interval(ms, toMsg)
}

module Runtime = {
  type state<'model, 'msg> = {
    mutable model: 'model,
    mutable subscriptions: array<Sub.t<'msg>>,
    mutable running: bool,
    mutable httpServer: option<Deno.HttpServer.t>,
    mutable intervals: array<Js.Global.intervalId>,
  }

  let rec executeCmd = async (cmd: Cmd.t<'msg>, dispatch: 'msg => unit) => {
    switch cmd {
    | Cmd.None => ()
    | Cmd.Batch(cmds) => {
        // Execute commands sequentially
        cmds->Js.Array2.forEach(c => {
          let _ = executeCmd(c, dispatch)
        })
      }
    | Cmd.Perform(task) => {
        let msg = await task()
        dispatch(msg)
      }
    | Cmd.PerformWithDispatch(fn) => await fn(dispatch)
    }
  }

  let make = (
    ~init: 'flags => ('model, Cmd.t<'msg>),
    ~update: ('msg, 'model) => ('model, Cmd.t<'msg>),
    ~subscriptions: 'model => Sub.t<'msg>,
    ~flags: 'flags,
  ) => {
    let (initialModel, initialCmd) = init(flags)

    let state = {
      model: initialModel,
      subscriptions: [],
      running: true,
      httpServer: None,
      intervals: [],
    }

    let rec dispatch = msg => {
      if state.running {
        let (newModel, cmd) = update(msg, state.model)
        state.model = newModel
        let _ = executeCmd(cmd, dispatch)
        updateSubscriptions()
      }
    }

    and updateSubscriptions = () => {
      let newSubs = subscriptions(state.model)
      // For now, just track that we have subs
      // In a full impl, we'd diff and manage lifecycle
      state.subscriptions = [newSubs]
    }

    and startSubscription = (sub: Sub.t<'msg>) => {
      switch sub {
      | Sub.None => ()
      | Sub.Batch(subs) => subs->Js.Array2.forEach(startSubscription)
      | Sub.HttpServer(port, handler) => {
          let server = Deno.serve(
            {"port": port},
            async (req) => {
              let body = await Deno.Request.text(req)
              let json = try {
                Some(Js.Json.parseExn(body))
              } catch {
              | _ => None
              }
              switch json->Belt.Option.flatMap(handler) {
              | Some(msg) => {
                  dispatch(msg)
                  Deno.Response.make("OK", {"status": 200})
                }
              | None => Deno.Response.make("Ignored", {"status": 200})
              }
            },
          )
          state.httpServer = Some(server)
        }
      | Sub.Interval(ms, toMsg) => {
          let id = Js.Global.setInterval(() => dispatch(toMsg()), ms)
          state.intervals = state.intervals->Js.Array2.concat([id])
        }
      }
    }

    // Start initial command
    let _ = executeCmd(initialCmd, dispatch)

    // Start subscriptions
    let initialSubs = subscriptions(state.model)
    startSubscription(initialSubs)

    // Return control functions
    {
      "dispatch": dispatch,
      "getModel": () => state.model,
      "stop": () => {
        state.running = false
        state.httpServer->Belt.Option.forEach(server => { let _ = Deno.HttpServer.shutdown(server) })
        state.intervals->Js.Array2.forEach(Js.Global.clearInterval)
      },
    }
  }
}
