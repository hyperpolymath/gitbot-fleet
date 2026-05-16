// ReScript bindings for Fetch API (available in Deno)

type response

@val external fetch: (string, {..}) => promise<response> = "fetch"

module Response = {
  @get external ok: response => bool = "ok"
  @get external status: response => int = "status"
  @get external statusText: response => string = "statusText"
  @send external json: response => promise<Js.Json.t> = "json"
  @send external text: response => promise<string> = "text"
}
