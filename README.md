# Otter 🦦

[Rendering compatibility](docs/rendering-compatibility.md)

## Declared HTTP errors

`ErrorPolicy` declares execution errors using ordinary Otter `Response` schemas. `Api` owns the global policy and an
ordered collection for documentation. Endpoints inherit the global policy; `.withErrors` declares the categories an
endpoint overrides:

```scala
val create = endpoint(createRequest, createResponse)
  .withErrors(ErrorOverrides(unexpected = Some(unavailable)))
val api = Api(errors, health, create)

val routes = Http4s.routes[IO](
  api,
  Route(health, healthHandler),
  Route(create, createHandler)
)(Http4sCirce.Payload)
val call = Http4s.client[IO, CreateInput, Created](api, create)(Http4sCirce.Payload, base, client)
val document = OpenApiRenderer.server(OpenApiProfile.V31, payload).render(info, api)
```

Consumers apply the global policy and endpoint overrides directly. Construction is total: there is no registration
lookup, endpoint resolution, or `Either` to unwrap before serving, calling, or rendering. The API's ordered collection
selects documentation entries; routes and clients may use any statically typed endpoint, including one outside that
collection. OpenAPI and TypeScript renderers both accept the API directly.

Handlers still return the domain result `B`. API-aware clients decode `Either[E, B]`, where `E` includes the decoded
error types from both the global policy and the endpoint's overrides. Payload requirements from both are checked by
the compiler. `Route(api, endpoint, handler)` also applies the same policy when building an individual route.

Without an API, an endpoint carrying overrides inherits `ErrorPolicy.default` for every omitted category. Standalone
routes, clients, and renderers all honor those overrides. Plain endpoint clients and renderers retain their domain-only
behavior. Explicit `ErrorPolicy(endpoint)` composition and `ComposedEndpoint` remain available.

Defaults have no body and decode to their `Status`. Envelope validation and payload syntax errors return 400,
unsupported content types return 415, and body schema validation returns 422. Entity-read, response-encoding,
status-conversion, unexpected execution, and defensive interpreter failures return 500. Causes remain diagnostic.
When request failures accumulate, envelope errors take precedence; a payload alternative with an eligible content
type supplies the syntax or validation failure instead of an ineligible alternative's content-type mismatch.

Replace policy entries with schemas, using `dimap` to construct a body from `Failure` while retaining its reader:

```scala
val unavailable = response(Status(503))(body.json(problemSchema))
  .dimap[Failure, Problem](_ => Problem.unavailable)(identity)
val errors = ErrorPolicy.default.copy(unexpected = unavailable)
```

The status, headers, and body schema are inspectable without running the mapping. A custom policy's payload
requirements are checked along with the endpoint's. See `sample-library`'s `api.contract` for a complete JSON policy.
Responses with identical wire representations cannot identify their originating failure category; domain alternatives
retain priority when decoding overlaps. OpenAPI and TypeScript preserve the declared response alternatives. OpenAPI
extracts complete response objects repeated by two or more operations into `components.responses`, keeping explicit
status codes and replacing the operation entries with `$ref` values; different overrides remain inline.

`Http4s.routes[IO](routes*)(payload, observation => ...)` optionally observes failures, cancellation, and failures
while producing an error response, with request and endpoint context. It defaults to a no-op. Observer failures
propagate; they do not select another response. A broken error response is reported and propagated without recursion.
Cancellation remains cancellation, unmatched routes fall through, and transport failures after handing the response
to http4s belong to the surrounding server middleware.

This replaces the former `malformed: Violations => Http4sWire.Response` callback and `Http4s.code`/`Http4s.malformed`
helpers. `Http4s.report` still formats violations for custom bodies. JSON syntax errors now default to 400 rather than
422, unsupported content types to 415, and default responses no longer include a plain-text report. Existing payload
codecs can override `decodeDetailed` to distinguish syntax from validation. It returns `DecodingFailure`, with a
required violation tree and an optional diagnostic cause; `decode` retains the violation-only API.
Extensible schema definition library for serialization formats (e.g. JSON, XML and CSV) with self-documenting API
definition capabilities.

You describe a value — or an HTTP endpoint — **once**, as a `Schema`. That one description is then read by different
interpreters: one parses and prints JSON, another CSV, another renders a JSON Schema, an OpenAPI document, or a
TypeScript module. A served route and its documentation come from the same value, so a document cannot drift away from
the server that answers it.

## Requirements

| | |
|---|---|
| JDK | 17+ |
| sbt | 2.0.8 (via the wrapper — `project/build.properties`) |
| Scala | 3.9.0 (only; no cross-building across Scala versions) |

## Modules

Every module publishes as `io.taig %% otter-<name>` and cross-builds to the JVM and Scala.js. Scala.js project ids
carry a `JS` suffix (`core-json-circeJS`).

**Core** — `core` is the schema algebra. Around it sit the format alphabets and their interpreters, paired: a module
naming a format's vocabulary, and a module interpreting it into a library's data model.

- `core-json` + `core-json-circe` / `core-json-borer` — JSON. circe is the better reader, borer the faster writer.
- `core-csv` + `core-csv-fs2-data` — CSV.
- `core-json-schema` — renders a schema as a JSON Schema document.
- `core-iron`, `core-java-time`, `core-case-insensitive` — vocabulary for refined types, `java.time` and `CIString`.
- `core-typescript`, `core-typescript-effect`, `core-json-typescript`, `core-json-typescript-effect` — TypeScript source
  model, printer and generators.

**HTTP** — `http` describes endpoints; it has no paired interpreter, because HTTP has no shared document model.

- `http-json`, `http-csv` — make a payload alphabet usable as a request or response body.
- `http-http4s` (+ `-circe`, `-fs2-data`) — serve and call endpoints over http4s.
- `http-openapi` — render endpoints as an OpenAPI 3.1 document.
- `http-typescript`, `http-typescript-effect` — render endpoints as TypeScript request descriptors.

`AGENTS.md` is the long-form design document: why each module exists, what the benchmarks say, and the sharp edges
worth knowing before writing against the library.

## Building and testing

```bash
sbt compile

sbt testJVM          # every module plus the sample
sbt testJS           # every module, Scala.js

sbt core-json-circe/testFull                                    # one module
sbt "core-json-circe/testOnly io.taig.otter.codec.JsonCirceDecoderTest"   # one suite
```

> **Use `testFull`, not `test`.** In sbt 2 `test` only runs what it thinks changed, and reports *"No tests to run"*
> after a clean.

Before pushing:

```bash
sbt testFull scalafmtCheckAll scalafixCheckAll blowoutCheck
```

Formatting is enforced (scalafmt, `maxColumn = 120`). Run `sbt scalafmtAll` before committing. The `.scalafmt.conf` and
`.scalafix.conf` files are generated by sbt-houserules and gitignored — edit the sbt settings, not the generated files.

## The sample project

`modules/sample-library` is the whole library used at once: a book-lending API defined as endpoint values, served over
http4s/ember, called back over those same values, and rendered as an OpenAPI document and a TypeScript module. It is
JVM-only and published nowhere.

### Run the server

```bash
sbt start          # alias for sample-library/run
```

Ember listens on `http://0.0.0.0:8080`. It serves eleven endpoints, seeded in memory with three books and two members:

```
GET     /health
GET     /books                      Every book the catalogue holds
POST    /books                      Add a book to the catalogue
GET     /books/{isbn}
PATCH   /books/{isbn}
DELETE  /books/{isbn}
POST    /books/{isbn}/scan
GET     /catalogue
POST    /intake
GET     /members/{reference}        A member, their membership and what they owe
POST    /members/{reference}/loans  Lend a book to a member
```

Three further endpoints — `POST /books/{isbn}/cover` (multipart), `GET /books/export` (ndjson stream) and
`GET /books/report` (CSV stream) — are described and documented but deliberately **not** served. They mark the current
shortfalls of the http4s backend, and `LibraryShortfallTest` asserts that each is reported rather than half served.

### Generate the documents

```bash
sbt "sample-library/runMain io.taig.otter.sample.Documents"
```

This is `runMain` rather than `run` because the module holds two `IOApp`s and `run` is pinned to the server. It writes
four files into `target/sample-library/` and prints each renderer's issues:

| File | What it is |
|---|---|
| `openapi.json` | OpenAPI 3.1, **server view** — what the running server accepts and returns |
| `openapi-client.json` | OpenAPI 3.1, **client view** — what a caller sends and expects |
| `api.ts` | TypeScript request descriptors |
| `book.schema.json` | JSON Schema, draft 2020-12 |

**The two OpenAPI documents are not redundant.** A server *reads* the request and *writes* the response; a client does
the reverse. Wherever a field is optional or carries a default the two disagree — the server accepts a `BookCreate`
without `metadata` because it fills in a default, while a generated client always sends one. Publishing only one of
them would misstate requiredness to half your audience. The `paths` are identical; every difference is inside
`components/schemas`.

Expect a handful of issues in the output. Renderers never throw and never half-emit: the three unserved endpoints are
reported by name and the rest of the document still comes back.

## Browsing the API in a GUI

The generated spec carries **no `servers` block** — the renderer emits `openapi`, `info`, `paths` and `components` only.
Add one before importing, or a GUI will resolve every path against its own origin:

```bash
python3 - <<'PY'
import json
d = json.load(open("target/sample-library/openapi-client.json"))
d["servers"] = [{"url": "http://localhost:8080"}]
json.dump(d, open("target/sample-library/openapi-client-local.json", "w"), indent=2)
PY
```

### Making requests — a native REST client

Use a desktop client such as [Bruno](https://www.usebruno.com), Insomnia or Postman, and import
`openapi-client-local.json` via *Collection → Import → OpenAPI V3*. Import the **client** document: it describes what a
caller sends, so generated request bodies carry the fields your client would actually produce.

Start the server with `sbt start` in another terminal first.

### Reading the docs — Swagger UI

```bash
docker run --rm -d --name otter-swagger -p 8081:8080 \
  -e SWAGGER_JSON=/spec/openapi-client-local.json \
  -v "$PWD/target/sample-library":/spec:ro swaggerapi/swagger-ui
open http://localhost:8081
```

Stop it with `docker rm -f otter-swagger`.

Swagger UI is good for **reading** the API, but its *Try it out* button will fail: the sample mounts no CORS
middleware, so a request from the browser page's origin to `localhost:8080` is blocked. Use a native client to
exercise the API.

## Documentation

- [AGENTS.md](AGENTS.md) — design rationale, module-by-module
- [Rendering compatibility](docs/rendering-compatibility.md) — what a renderer can and cannot express, and where
  Scala-side validation is still required

## License

MIT — see [LICENSE](LICENSE).
