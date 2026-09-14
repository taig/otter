# Otter 🦦

[Rendering compatibility](docs/rendering-compatibility.md)

## Declared HTTP errors

`ErrorPolicy` declares execution errors using ordinary Otter `Result` schemas. Compose the endpoint once and use its
effective contract for clients and documentation:

```scala
val served = ErrorPolicy.default(myEndpoint)

val routes = Http4s.routes[IO](Http4sCirce.Payload)(Route(served, myHandler))
val clientEndpoint = served.effective
```

The handler still returns the domain result `B`. An Otter client reading `served.effective` returns `Either[E, B]`,
where `E` is the policy's decoded error type. Pass the same effective endpoints to the OpenAPI and TypeScript
renderers. Plain `Route(endpoint, handler)` uses the default policy; compose explicitly when sharing its error contract.

Defaults have no body and decode to their `Code`. Envelope validation and payload syntax errors return 400,
unsupported content types return 415, and body schema validation returns 422. Entity-read, response-encoding,
status-conversion, unexpected execution, and defensive interpreter failures return 500. Causes remain diagnostic.
When request failures accumulate, envelope errors take precedence; a payload alternative with an eligible content
type supplies the syntax or validation failure instead of an ineligible alternative's content-type mismatch.

Replace policy entries with schemas, using `dimap` to construct a body from `Failure` while retaining its reader:

```scala
val unavailable = result(Code(503))(body.json(problemSchema))
  .dimap[Failure, Problem](_ => Problem.unavailable)(identity)
val errors = ErrorPolicy.default.copy(unexpected = unavailable)
val served = errors(myEndpoint)
```

The status, headers, and body schema are inspectable without running the mapping. A custom policy's payload
requirements are checked along with the endpoint's. See `sample-library`'s `api.contract` for a complete JSON policy.
Responses with identical wire representations cannot identify their originating failure category; domain alternatives
retain priority when decoding overlaps. OpenAPI and TypeScript preserve the declared response alternatives.

`Http4s.routes[IO](payload, observe = observation => ...)` optionally observes failures, cancellation, and failures
while producing an error response, with request and endpoint context. It defaults to a no-op. Observer failures
propagate; they do not select another response. A broken error response is reported and propagated without recursion.
Cancellation remains cancellation, unmatched routes fall through, and transport failures after handing the response
to http4s belong to the surrounding server middleware.

This replaces the former `malformed: Violations => Http4sWire.Response` callback and `Http4s.code`/`Http4s.malformed`
helpers. `Http4s.report` still formats violations for custom bodies. JSON syntax errors now default to 400 rather than
422, unsupported content types to 415, and default responses no longer include a plain-text report. Existing payload
codecs can override `decodeDetailed` to distinguish syntax from validation. It returns `DecodingFailure`, with a
required violation tree and an optional diagnostic cause; `decode` retains the violation-only API.
