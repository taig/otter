# Otter

Extensible schema definition library for serialization formats (e.g. JSON, XML and CSV) with self-documenting API definition capabilities.

## Development workflow

Modules: `core`, `core-json`, `core-json-circe`, `core-json-schema`, `core-csv`, `core-csv-fs2-data`, `core-iron`,
`core-java-time`, `core-case-insensitive`, `core-typescript`, `core-typescript-effect`, `core-json-typescript`,
`core-json-typescript-effect`, `http`, `http-json`, `http-openapi`.
Each cross builds to the JVM and Scala.js; the Scala.js project ids carry a `JS` suffix
(`core-json-circeJS`).

`core-json`/`core-json-circe` and `core-csv`/`core-csv-fs2-data` are the same pair twice: a
module defining a format's alphabet, and a module interpreting it into a library's data model.

`core-json-schema` renders a JSON schema as a JSON Schema document. It is one module rather than a pair because a
JSON Schema *is* a JSON document and `core-json-circe` already says what one of those is, so there is no second
library's data model to interpret into. What varies is the consumer -- draft 2020-12, a strict structured output
profile -- and that is a `JsonSchemaProfile` value rather than a module. A renderer is given a `Side`: the document you
hand a producer is the side you will read, and the two differ wherever a field is optional or holds a default.

`http` describes HTTP endpoints. It is an alphabet like `core-json` and `core-csv`, but it needs no paired
interpretation module, and where the pairing breaks is the whole design. `core-json`/`core-json-circe` works because
circe supplies a *document data model* -- a pure, total value both sides can name. HTTP has no such shared model: every
backend has its own request type, and a request is not a pure value because its body arrives over time. So the split
runs through the middle of a request rather than between two modules:

- The **envelope** -- method, path, query string, headers -- is text and nothing more, so `http` ships the pure
  `Encoder`/`Decoder` instances for it. Their `T` is the narrow wire slice each concerns (`Vector[String]` for path
  segments, `Chain[(String, Option[String])]` for a query string, `Chain[(String, String)]` for headers), so path
  matching, parsing and violation paths are written once and a backend adapts its own types with a few trivial
  functions.
- The **body** gets no representation here at all. `http` says what a body is -- a media type over a payload schema,
  over bytes, or over a framed sequence of elements -- and stops. Interpreting one is a backend's business, and its
  signatures may freely mention `F[_]` and that backend's stream type. There is no `Request.Data`, and so no
  `Array[Byte]` field to force a request to be buffered before it can be routed.

Almost every tier is an existing core node rearranged, which is why the module is small: a path is a `Tuple` of
segments, a static segment is a `Constant` (it writes its literal, requires it on read, and erases to `Unit`, so
`Append` drops it), a dynamic one is a `Branch`, a query string and a header set are `Record`s of `Field`s, and
alternatives -- `Bodies`, `Results` -- are `Union`s. `Multipart` is a `Record` of `Part`s, which is the *product* of
bodies that makes a file upload describable; a multipart body is not a case of `Body` but a payload for one, exactly as
it is in HTTP. A streamed body names its element and its framing and contributes nothing to what the endpoint holds:
what a sequence of elements *is* belongs to whoever has an effect type to say it in, and `Body.Streamed.Schema` keeps
the element type so a backend can pin it in the compiler.

`Endpoint.Server` and `Endpoint.Client` are `Side` one tier up: a server reads the request and writes the response, and
a caller does the reverse. The two differ wherever a field is optional or holds a default, so the same endpoint value
renders as two different documents.

`http-json` makes a JSON schema usable as a body payload, and is the shape any second payload alphabet takes.
`http-openapi` renders endpoints as an OpenAPI 3.1 document -- a renderer rather than a pair, on the same reasoning
`core-json-schema` is one, with `OpenApiProfile.V31` as the `JsonSchemaProfile` value that says which JSON Schema its
schemas are written in. Payload alphabets are rendered by an `OpenApiPayload`, which dispatches at runtime because a
payload's alphabet is existential by the time a renderer holds one; an alphabet it does not recognise is reported as an
issue and the body is still listed.

The four typescript modules are a lattice, not a chain: `core-typescript` is the TypeScript source
model and printer, `core-typescript-effect` the vocabulary of one target library, `core-json-typescript`
everything a JSON renderer needs whatever the target (including the recursion fixpoint), and
`core-json-typescript-effect` the generator itself. A second target -- zod, say -- is a
`core-typescript-zod`/`core-json-typescript-zod` pair beside the two `-effect` ones.

### Fast loop

Stay in one project.

```
# Compile to check for errors quickly
sbt core-json-circe/compile

# Run tests for the current project
sbt core-json-circe/testFull
sbt "core-json-circe/testOnly io.taig.otter.codec.JsonCirceDecoderTest"
```

`test` in sbt 2 only runs what it thinks changed, and reports "No tests to run" after a
clean. Use `testFull` to actually run a suite.

Note that zio-test's Scala.js runner under-reports its summary count: the per-test `+` lines
are the truth, not the "N tests passed" line.

### Before pushing

```
sbt testFull scalafmtCheckAll scalafixCheckAll blowoutCheck
```

## Concatenation

Products are built with two operators. `:*` is left-associative and carries what it has built on the left; `*:` is
right-associative and carries it on the right, so `TNil :* foo :* bar` and `foo *: bar *: TNil` are one schema.
`Append` and `Prepend` are the match types that keep each flat, and each drops a `Unit` operand, which is how a static
path segment stays out of a path's value type.

Neither needs an empty root: two schemas beside each other already are the container that holds them, so `foo :* bar`
and `foo *: bar` say the same thing, and `TNil`, `RNil`, `PNil`, `QNil` and `HNil` are places to start rather than
requirements. What keeps a root-less instance off the toes of the one for a receiver that already is a container
differs per alphabet -- a cell is not a row, a segment is not a path, and JSON, which has no such tier, asks
`NotGiven` instead.

`*:` reuses every `AppendableOperation` that `:*` does, because the type class names a container and an element rather
than a left and a right. What it gives up is the by-name element: the left operand of a right-associative operator is
the extension parameter, and Scala evaluates it first.

`++` is `zip` written as an operator. It concatenates what the container holds like the other two -- `record ++ record`
writes both sets of fields into one object -- and differs in the Scala value, which stays a pair rather than flattening,
because neither operand is a member of the other. It binds tighter than `:*` and looser than `*:`.

## Code Style

- Scalafmt enforced (maxColumn: 120, Scala 3 dialect)
- No comments explaining obvious code changes
- Follow existing patterns in neighboring files
- Fully qualified namespaces: When referring to a type, always start with the root type of the current file instead of using relative references
- Never omit the `override` keyword

## Boundaries

### Always do

- Compile and run tests after modifying code
- Apply Scalafmt by running `sbt scalafmtAll`

### Ask first

- Adding new dependencies (even test-only)
- Creating new subprojects
- Changing build configuration
- Modifying CI workflows

### Never do

- Add dependencies
- Commit without formatting
- Delete or skip tests to make CI pass