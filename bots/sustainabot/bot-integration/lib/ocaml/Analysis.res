// Analysis service client

open Types

// Fetch analysis from the analysis service
let analyzeRepository = async (endpoint: string, repoUrl: string, ref: string): result<
  analysisResult,
  string,
> => {
  let body = Js.Json.object_(
    Js.Dict.fromArray([("url", Js.Json.string(repoUrl)), ("ref", Js.Json.string(ref))]),
  )

  try {
    let response = await Fetch.fetch(
      `${endpoint}/repository`,
      {
        "method": "POST",
        "headers": {"Content-Type": "application/json"},
        "body": Js.Json.stringify(body),
      },
    )

    if Fetch.Response.ok(response) {
      let json = await Fetch.Response.json(response)
      // In production, would properly decode the JSON
      Ok(Obj.magic(json))
    } else {
      Error(`Analysis failed: ${Fetch.Response.statusText(response)}`)
    }
  } catch {
  | Js.Exn.Error(e) =>
    Error(`Analysis request failed: ${Js.Exn.message(e)->Belt.Option.getWithDefault("unknown")}`)
  }
}

let analyzeDiff = async (
  endpoint: string,
  repoUrl: string,
  base: string,
  head: string,
): result<analysisResult, string> => {
  let body = Js.Json.object_(
    Js.Dict.fromArray([
      ("url", Js.Json.string(repoUrl)),
      ("base", Js.Json.string(base)),
      ("head", Js.Json.string(head)),
    ]),
  )

  try {
    let response = await Fetch.fetch(
      `${endpoint}/diff`,
      {
        "method": "POST",
        "headers": {"Content-Type": "application/json"},
        "body": Js.Json.stringify(body),
      },
    )

    if Fetch.Response.ok(response) {
      let json = await Fetch.Response.json(response)
      Ok(Obj.magic(json))
    } else {
      Error(`Diff analysis failed: ${Fetch.Response.statusText(response)}`)
    }
  } catch {
  | Js.Exn.Error(e) =>
    Error(`Analysis request failed: ${Js.Exn.message(e)->Belt.Option.getWithDefault("unknown")}`)
  }
}

// Mock analysis for testing
let mockAnalysis = (): analysisResult => {
  {
    eco: {
      carbonScore: 72.0,
      energyScore: 68.0,
      resourceScore: 75.0,
      score: 71.5,
    },
    econ: {
      paretoDistance: 0.15,
      allocationScore: 80.0,
      debtScore: 65.0,
      score: 72.0,
      paretoStatus: Some({
        isOptimal: false,
        distance: 0.15,
        improvements: Some(["Reduce complexity in src/utils.rs", "Add memoization to hot path"]),
      }),
    },
    quality: {
      complexityScore: 70.0,
      couplingScore: 75.0,
      coverageScore: 82.0,
      score: 75.5,
    },
    health: {
      eco: 0.4,
      econ: 0.3,
      quality: 0.3,
      total: 72.8,
      grade: "C",
    },
    violations: [],
    recommendations: [
      {
        entityId: "src/processing.rs",
        action: "optimize_loop",
        reason: "Hot loop could benefit from vectorization",
        priority: PriorityMedium,
        confidence: 0.78,
        expectedImprovement: Js.Dict.fromArray([("carbonScore", 5.0), ("energyScore", 8.0)]),
      },
    ],
    timestamp: "2024-12-08T10:00:00Z",
    commitSha: Some("abc123"),
  }
}
