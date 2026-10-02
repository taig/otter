# Otter

Extensible schema definition library for serialization formats (e.g. JSON, XML and CSV) with self-documenting API definition capabilities.

## Project maturity

Otter is a highly experimental work in progress. When making changes or reviewing the design, prefer fundamental
improvements—even when they require breaking APIs—over workarounds that preserve prior design mistakes. Do not treat
existing APIs or architecture as settled when a cleaner design would produce a better result.

## Development workflow

Modules: `core`, `core-json`, `core-json-borer`, `core-json-circe`, `core-json-schema`, `core-csv`,
`core-csv-fs2-data`, `core-iron`, `core-java-time`, `core-case-insensitive`, `core-typescript`,
`core-typescript-effect`, `core-json-typescript`, `core-json-typescript-effect`, `http`, `http-json`, `http-csv`,
`http-http4s`, `http-http4s-circe`, `http-http4s-fs2-data`, `http-openapi`, `http-typescript`,
`http-typescript-effect`.
Each cross builds to the JVM and Scala.js; the Scala.js project ids carry a `JS` suffix
(`core-json-circeJS`).

`sample-library` is apart from them the way `benchmark` is: JVM only, published nowhere, not one of the cross built
`modules`. Unlike `benchmark` it has tests worth running, so it is appended to the `testJVM` alias by hand.

`benchmark` is apart from all of them: JVM only, published nowhere, and not run by `testJVM`. It holds the JMH
benchmarks that say where a read and a write actually spend their time -- `sbt "benchmark/Jmh/run -wi 3 -i 5 -f 1"`,
and add `-prof gc` for `gc.alloc.rate.norm`, which is deterministic and so says more than a timing does. It measures
the JSON fixtures in `core-json`'s test sources, which is why it depends on them.

How to read a result. On **reads**, `parseText` is the document model on its own. On **writes** there are two halves
and neither `encodeDocument` nor `printDocument` is one of them: `encodeDocument` walks the schema *and* builds the
tree, so `encodeVoid` -- the same walk with nothing built, via `JsonVoidEncoder` -- is what you subtract.
`(encodeDocument - encodeVoid) + printDocument` is the document model's share of a write.

Measured, that share is **64% of a flat record's write, 79% of a nested document's and ~95% of a small one**. The
schema interpreter is the other 14-29% of a write and allocates almost nothing (24B to 1.2KB per op, against 5.4KB to
39KB for the whole write). That asymmetry is why `core-json-borer` exists and why its encoder carries a deferred write
rather than a document.

Reads are the other way round: the interpreter costs 493ns and 3.3KB for a fifteen field record, so parsing is 21% to
53% of a read, and for a small record the read is essentially the parse.

**Read allocation before you read timings, and only count allocation that escapes.** `-prof gc`'s
`gc.alloc.rate.norm` is exact -- it reproduces to the byte across runs and fork counts -- but it is measured *after*
JIT, so escape analysis has already deleted every allocation it could prove non-escaping. Counting `new`s in the source
therefore over-states the prize, and the two changes that paid here both removed *escaping* objects: a hash index
stored into a `HashMap`, and a tuple returned up a recursive `decodeRemaining`, which recursion keeps the JIT from
inlining through.

The corollary is sharper, and cost two commits to learn: **removing allocations from the source can make things
worse.** Hand-inlining cats combinators at the primitive leaf added 224B to a record and 3.5KB to a tree; guarding
`Metadata.get` on an empty map added 168B to a small record and 22% to its time. cats' combinators are small, inline,
and their garbage is scalar-replaced; a hand-written generic helper is megamorphic across its call sites, does not
inline, and its result escapes. Both were reverted. Measure each change on its own and keep only what the number
supports.

`core-json` has two interpreters, and they are worth comparing. `core-json-circe` builds an `io.circe.Json` both ways.
`core-json-borer` reads through borer's `Dom` -- a schema driven read needs random access, so a streaming reader
cannot do it, which is the finding `JsonBorer.decoder` documents -- but writes straight to bytes through `BorerWrite`,
a `Writer => Writer` with a left to right `Monoid`, so no document is built on the way out. The encoder combinators in
`core` are generalised over that `Monoid` for exactly this reason: circe instantiates them at the `Chain`/`Vector`
containers it has always used, borer at a type where combining is composition and there is no spine.

Which one to reach for is a measured trade rather than a preference. **borer writes 18% to 54% faster and allocates
42% to 77% less**, the win growing with nesting because a document model charges per node and a deferred write does
not. **borer reads 6% to 17% slower**, and that is not the adapter: it is that `Dom.MapElem` keeps its members in an
`Array[Element]` with each key wrapped in a `StringElem`, so reading a record unwraps a key per member where circe's
`JsonObject` is already `String` keyed. So borer is the clear choice where writes dominate, and circe stays the better
reader. Both are the same alphabet, so a caller can use one of each if the traffic is lopsided enough to care.

`core-json`/`core-json-circe`, `core-json`/`core-json-borer` and `core-csv`/`core-csv-fs2-data` are the same pair
three times: a module defining a format's alphabet, and a module interpreting it into a library's data model.

`core-json-schema` renders a JSON schema as a JSON Schema document. It is one module rather than a pair because a
JSON Schema *is* a JSON document and the interpreter modules already say what one of those is, so there is no second
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
alternatives -- `Bodies`, `Responses` -- are `Union`s. `Multipart` is a `Record` of `Part`s, which is the *product* of
bodies that makes a file upload describable; a multipart body is not a case of `Body` but a payload for one, exactly as
it is in HTTP. A streamed body names its element and its framing and contributes nothing to what the endpoint holds:
what a sequence of elements *is* belongs to whoever has an effect type to say it in, and `Body.Streamed.Schema` keeps
the element type so a backend can pin it in the compiler.

`Endpoint.Server` and `Endpoint.Client` are `Side` one tier up: a server reads the request and writes the response, and
a caller does the reverse. The two differ wherever a field is optional or holds a default, so the same endpoint value
renders as two different documents.

`http-json` makes a JSON schema usable as a body payload, and is the shape any second payload alphabet takes. A
`Multipart` payload carries its parts' requirements too: `Multipart.Schema`'s parameter is the *body* each part holds
and a body keeps its own requirement, so `:*` was always accumulating them -- `Multipart.Over[S]` is the name a
definition is ascribed at, and `Multipart.Requirement[S]` what it asks of an interpreter. The `Multipart[A]` and
`Part[A]` aliases take the requirement, as every other alias in `http` does: widening a part to `Body.Node`, whose
requirement is `Any`, would make an upload servable by nothing.
`http-openapi` renders endpoints as an OpenAPI 3.1 document -- a renderer rather than a pair, on the same reasoning
`core-json-schema` is one, with `OpenApiProfile.V31` as the `JsonSchemaProfile` value that says which JSON Schema its
schemas are written in. Payload alphabets are rendered by an `OpenApiPayload`, which dispatches at runtime because a
payload's alphabet is existential by the time a renderer holds one; an alphabet it does not recognise is reported as an
issue and the body is still listed.

`http-http4s` runs one. Not the paired interpretation module the section above says HTTP cannot have -- there is still
no shared document model -- but the demonstration that the seam was cut in the right place: `Query` is
`Vector[(String, Option[String])]` in both directions and `Headers` a list of raw name and value pairs, so the envelope
crossing is a container conversion and `Http4sEnvelope` is the whole adapter. It is http4s 1.0 rather than 0.23 for
`Entity`, which is a sealed `Strict(ByteVector) | Streamed(body, length) | Empty` whose first and last are
`Entity[Pure]`: a whole body is bytes already held, so it reaches a codec with no effect type in sight, and only a
streamed one ever needs `F`. Nothing is buffered before a route matches -- routing reads the method and the path, and
the entity is touched afterwards and only if the endpoint describes a body.

Three things there are worth reading before extending it. `Http4sPayload` is *not* `OpenApiPayload`, and the
difference is the point. A renderer holds a payload whose alphabet is genuinely existential, so it dispatches at
runtime and reports what it does not recognise. A backend does not: the requirement `S` that every body, endpoint and
route carries accumulates through `Body.Or` and is checked where the routes are built, so an alphabet that reaches the
interpreter is one somebody registered. `Http4sPayload` is therefore *total* -- `decode[R](payload: P[Nothing, R],
bytes)` returns a `Validated` and not an `Option[Validated]` -- and the walks are written at `Supported[P]` rather than
at `Body.Node`, which is what lets `Body.Value.Whole` hand its payload over as a `P`. Scala does invert
`Whole[S] extends Body.Value[Body.Whole[S], W, R]` to recover `S`.

The one runtime question left is which side of a `P[w, r] | Q[w, r]` a payload is, and `Http4sPayload.Alphabet` is
where it is asked. It is phrased as a choice -- "mine or theirs" -- rather than as `Option`, so the second branch needs
no test of its own and there is no "neither" for anybody to answer. That is what makes `orElse` compose two total
interpreters into a total one, and it is the single place the erasure is crossed: a pattern match inside the instance,
which `DisableSyntax.asInstanceOf` would refuse in any case.

A streamed body and a request carrying one are not cases in those walks at all. Their requirement is
`Body.Requirement.Streamed`, which no `Supported[P]` admits, so the compiler reports the branches as unreachable and
the endpoints as unservable. `Http4sIssue` therefore has one case: a body this interpreter carries and this value
gave it nothing to write. `Http4sFailure.Encoding` is what raises it.

Each `Failure.Category` is raised by something a request or a handler does, and there is none for a body nothing can
read, because no request can reach one. Add a category only for a failure some backend selects: one none selects would
be a response every consumer had to declare and none would ever send.

And a router asks a different question from `PathDecoder`: arity and literals only, via `PathTemplate`, because a
decode failure cannot tell "some other endpoint" (fall through, 404 or 405) from "this endpoint, called wrongly"
(stop, 400).

`Http4s.routes` still falls through, because that is what composes with `<+>`, but falling through destroys the one
fact only the router has: that the path is one some route spells, under other methods. `Http4s.app` is the terminal
form that keeps it -- a `405` carrying `Allow` where a route spells the path, a `404` otherwise -- and
`Http4s.fallback` is that answer alone, as total `HttpRoutes`, for composing last behind foreign routes. Both write
through `Api.unrouted`, an `UnroutedPolicy` beside `errors` and not in it: an unrouted request belongs to no endpoint,
so a `Failure.Category` for it would be the removed `Interpreter` again. The policy is write only and has no `E`,
because nothing decodes or documents it -- a client meets one only when it and the server disagree about what exists,
and OpenAPI has no response that belongs to no operation -- which is also what lets `UnroutedPolicy.default` be
bodyless beside any error type. Its requirement still joins the `Api`'s `S`, so whatever serves or calls the `Api`
must cover it; the renderers take any `Api` and check nothing. `Allow` is written by the interpreter on every `405`,
replacing any the declaration wrote, and lists what is routed and never what is only documented; a fallback asks only
the routes it is given. `HEAD` is not synthesised, `OPTIONS` is not answered, and an unknown method on a path some
route spells is a `405` rather than a `501`. `DefaultHead` retries a `HEAD` as a `GET` only when what it wraps falls
through, so it wraps `routes` with `fallback` composed after it -- one more reason `routes` must keep falling through.

`Http4s.routes` takes its routes first and its interpreter after them, which is the direction the requirement flows:
the routes say what is needed, and the interpreter argument is what fails to typecheck when it falls short. Naming the
interpreter first settled what could be served before a route had been read. `observe` is a second overload of each
shape rather than a default argument, because Scala permits defaults on only one variant of an overloaded method.

It carries the envelope, `Body.Whole` and `Body.Binary`, both sides. `http-http4s-circe`
supplies the JSON payload, and is a module of its own where `http-openapi` needed none: there circe *is* the document
model a JSON Schema is, here it is one of two interpreters of one alphabet and which to reach for is the caller's
measured trade. `Http4sRoundTripTest` is what the module rests on -- `Client.fromHttpApp` over the same endpoint value
read as both sides, so neither half can agree by being written twice the same wrong way, and no socket means it runs on
Scala.js too.

`http-csv` is the second payload alphabet, beside `http-json`: a `CsvDocument` is exactly one row or a finite collection
of rows. `http-http4s-fs2-data` interprets it for http4s, buffered, with fs2-data writing the CSV wire syntax and
Otter's schema writing each row, as `http-http4s-circe` does for JSON.

The four typescript modules are a lattice, not a chain: `core-typescript` is the TypeScript source
model and printer, `core-typescript-effect` the vocabulary of one target library, `core-json-typescript`
everything a JSON renderer needs whatever the target (including the recursion fixpoint), and
`core-json-typescript-effect` the generator itself. A second target -- zod, say -- is a
`core-typescript-zod`/`core-json-typescript-zod` pair beside the two `-effect` ones.

`http-typescript`/`http-typescript-effect` are that lattice one tier up, and split where it splits: what
an endpoint contributes to generated source -- the shape of its input, the pieces of its path and query
string, which status codes it answers under -- is the same whatever library reads its payloads, and only
the payloads are written in a target's vocabulary. `TypescriptPayload` is `OpenApiPayload` again, for the
same reason and with the same runtime dispatch, and `TypescriptIssue` carries `OpenApiIssue`'s contract:
what cannot be said is recorded and a module still comes back.

**What is generated is a descriptor and not a client, and that is the whole design.** A generated function
that fetched and decoded before returning would never let its caller hold the response as the serialisable
thing it arrived as, and a cache that requires serialisable values -- Next.js' is the one that prompted
this -- cannot keep what a schema decoded into a `Date`. So a descriptor carries the builders that write a
request out of an input, the `Schema` for each body keyed by media type, and *both* the decoded and the
encoded type of every answer: a caller names their cache at the encoded type and decodes past it, and when
to decode is theirs. Nothing generated calls `fetch`, and `TypescriptEndpointRendererTest` asserts that
outright rather than leaving it to be noticed. A streamed body and a `Multipart` payload are reported
rather than half emitted, which is the stand `http-http4s` already takes.

Response headers are deliberately not in a generated answer type. A descriptor decodes no headers -- they
are text the caller reads off the `Response` -- and a type claiming a header is a `number` would be
describing a value nothing produces.

The nodes of `core-typescript` a descriptor needs are `Expression.Function` (an arrow whose parameters
carry types), `Index` (how a member is read, since every key the printer writes is quoted), `Undefined`,
`AsConst`, `Statement.Import` and `Type.Function`. The printer parenthesises an arrow whose body is an
object literal, because
`(value) => { "a": 1 }` is a function whose body is a labelled statement and not one returning an object.
The two parse differently rather than merely looking different, which is the kind of mistake the source
model exists to prevent and which building strings would never have caught.

The walks over a `Record` or a `Union` that both renderers need -- `Queries.fields`, `Headers.fields`,
`Multipart.parts`, `Bodies.branches`, `Responses.branches` -- live in `http` beside `Path.segments` rather
than privately in each renderer, on the reasoning `Path.segments` already records: what is asked of a
record is always the list of its leaves, and its shape says nothing a caller wants to know.

`sample-library` is the whole tier used at once: a library management API defined, served over ember, called back over
the same endpoint values, and rendered as an OpenAPI document and a TypeScript module. It exists because the fixtures
cannot be that. Each of them is shaped by what its own suite had to ask, and a reader wanting to know how a domain type
becomes a served route has to work out which test's needs bent which fixture first. Nothing in it is measured against
another module's fixture, and it names `zio-test` itself rather than taking a `test->test` edge, so no other module's
`api` or `json` object is in scope to be mistaken for its own.

What it deliberately does *not* serve is the more useful half. `POST /books/{isbn}/cover` carries a `Multipart`
payload, `GET /books/export` answers with an ndjson stream, and `GET /books/report` answers with a stream whose
elements are written in the CSV alphabet -- one payload no interpreter recognises, one shape no interpreter carries,
and one of each. All three are rendered into both documents and reported by name there; the http4s backend refuses all
three where the routes are built, and `LibraryShortfallTest` asserts that of the compiler rather than of a response.
Those tests failing is the signal that a shortfall has been fixed.

Two product conversion details are worth knowing before writing anything against this library.

**Opaque domain types need no representation bound.** `Append.Shape` and `Prepend.Shape` carry an `Out` type selected
by the same implicit search as their `split` and `join` operations. A separate match type would get stuck on an
unbounded opaque member because the compiler cannot prove it disjoint from `Unit` or a tuple; the evidence instead
selects a scalar shape without inspecting its representation. Only a visible `Unit` is dropped and only a visible tuple
accumulator is flattened. The sample's `opaque type Isbn = String` therefore stays opaque, and its `Ordering` is supplied
by its domain API rather than inherited from `String`. Generic combinators must pass the shape evidence through to keep
the shape selected at their call site.

**A bodyless response can name its domain case with `.to`.** `Convert.product0` maps `Unit` to a parameterless
case, so `response(status.noContent).to[Deleted.Removed.type]` names a successful deletion without adding an entity.
Convert each branch to its case before converting the union to the enum. `books.delete` is the example with a bodyless
success and a JSON conflict; `books.create` and `loans.borrow` carry bodies in every branch. `books.fetch` keeps
`Option[Book]` because absence is what that endpoint means. These conversions preserve the body requirements and the
HTTP contract in both directions. All-bodyless alternatives keep `Nothing` through specialized `AlternableOperation`
instances in `Response` and `Responses`: applying `Body.Or` to two bottom constructors otherwise exposes a Scala
kind-checking failure. The general instances have lower priority and still accumulate requirements as soon as a body
is present. No conversion changes the body requirement.

**A request names its input with `.to` too**, since `Queries`, `Headers` and `Request.Schema` are profunctors
like any other schema. `books.list` names its query string `BookFilter` and its headers `Tracing` where each is
defined, so its input is `(BookFilter, Tracing)`. Name the parts before composing them: `Append` adds a named value as
a single member, while a `.headers` added after a request's `.to` goes beside the named value rather than inside it.
The handler untuples that pair, `(filter, _) => library.list(filter)`, and it can only because `Route` has exactly one
`apply` per arity. Scala types a lambda against its expected type only once overloading has settled, so a second
two-argument `apply` leaves `(filter, _) => ...` nothing to untuple against. That is why `Route`'s constructor is
private outright, since a case class constructor is a candidate wherever it is visible, `private[http]` included, and
why a `ComposedEndpoint` is served through `Route.composed`. `Convert` matches by position and type, not by name, so a case class declaring two `Int`s in the
wrong order still compiles. Names protect every call site, not the one declaration. `LibraryFilterContractTest`
asserts that the wire and both documents are unchanged.

A third thing it records rather than leaves to be discovered: **a placeholder shadows a literal of the same arity**.
`/books/{isbn}` matches `/books/export` on arity and on its one literal, so whichever is registered first wins, and a
literal path must come before the placeholder path that would otherwise swallow it. The shadowing decides `Allow` too:
`GET` and `DELETE /books/export` are both `400`s for an ISBN that does not parse, and `PUT /books/export` is a `405`
naming the methods of `/books/{isbn}`, exactly as `PUT /books/{isbn}` is.

### Fast loop

Stay in one project.

```
# Compile to check for errors quickly
sbt core-json-circe/compile

# Run tests for the current project
sbt core-json-circe/testFull
sbt "core-json-circe/testOnly io.taig.otter.codec.JsonCirceDecoderTest"
```

What a JSON interpreter owes is written down once, in `core-json`'s test sources, and every interpreter is held to
it. `JsonInterpreter` names the three points -- read a document, write one, round trip a value -- and
`JsonDecoderSuite`, `JsonEncoderSuite` and `JsonRoundTripSuite` are the claims. A module's own suite is the one line
that binds them (`object JsonCirceEncoderTest extends JsonEncoderSuite(JsonCirceInterpreter)`) plus an `extra` list for
what the contract structurally cannot ask about.

A document is **text** on both sides of the contract, which is the only form every interpreter has in common:
`core-json-borer` builds no document on the way out at all. Reading text means reading it through the interpreter's own
parser, so the contract holds the bridge and the parser together rather than the interpreter alone -- which is the
intent, since bytes are what a caller actually hands a JSON library.

`Doc` is a separate thing and is *not* what the contract uses. It is the document as text for a **differential** test,
where the same document has to reach two interpreters in two models and neither may be derived from the other -- that
derivation is the code under test, and its bugs would cancel out. It lives in `core-json` rather than beside either
interpreter so that a third one can write its own differential test without depending on a second one's test sources;
`CirceDoc` and `BorerDoc`, which turn it into a library's model, live beside the library.

Three kinds of claim live in three places, and it is worth knowing which is which before adding a test:

- **The contract**, in `core-json`. What the alphabet says, stated absolutely, over the documents a person wrote down.
  It is what an interpreter with no oracle to compare against is measured by.
- **Agreement**, in `core-json-borer`. `JsonBorerAgreementTest` reads every fixture schema over its canonical document
  and one edit at a time, asserting borer and circe produce the *same* `Validated[Violations, A]`;
  `JsonBorerEncodeAgreementTest` does the same for what they write, over generated values. Together they cover a corpus
  far wider than anything written down, and compare whole violation trees -- but only relative to circe.
- **A module's own**, in that module. What neither can reach: the bridge objects (`JsonCirceTest`, `JsonBorerTest`),
  the number ladder, hand built `Dom` values no parser produces, `Float` spelling and whether a renderer refuses a NaN,
  and the documented differences in `JsonBorerDivergenceTest`.

Duplicate keys are outside the contract on purpose rather than configurable in it. borer's record reads the first
occurrence where circe's `JsonObject` has already kept the last, and JSON does not say which is right; a contract with
a knob per difference would have stopped asserting anything. It is stated the other way round, per module, instead.

The same holds for a schema that *declares* one name twice, which is legal and not a mistake to be rejected. `Fields`
hands out the first occurrence nothing has claimed, so `field("x", int) :* field("x", int)` reads a document in arrival
order; `RecordEncoder` writes a member per declaration and `BorerWrite`'s `Monoid` is left to right, so borer writes
`{"x":1,"x":2}` and reads `(1, 2)` back. Checking field names for uniqueness would delete that.

circe cannot do it in either direction, and that is its data type rather than a setting: a `JsonObject` is a
`LinkedHashMap` keyed by name, so the write collapses to the last and the second field then reads as missing.
`JawnParser`'s `allowDuplicateKeys` is not the knob it sounds like -- it chooses between keeping the last and refusing
the document outright, and neither preserves one. `JsonBorerDivergenceTest` states both halves.

`DirectionTest`, `FlatnessTest` and `ZipTest` are not part of the contract and are not mirrored: they assert properties
of the schema algebra through `compiletime.testing.typeChecks` and mention an interpreter only as an arbitrary witness.
One witness suffices.

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
`Append` and `Prepend` use shape evidence to keep each flat, and each drops a visible `Unit` operand, which is how a static
path segment stays out of a path's value type.

Neither needs an empty root: two schemas beside each other already are the container that holds them, so `foo :* bar`
and `foo *: bar` say the same thing, and `TNil`, `RNil`, `QNil`, `HNil` and `Path.Root` are places to start rather than
requirements. What keeps a root-less instance off the toes of the one for a receiver that already is a container
differs per alphabet -- a cell is not a row, a segment is not a path, and JSON, which has no such tier, asks
`NotGiven` instead.

The path tier is the one whose root is not named after the product it is. It is `Path.Root`, and `HttpComponent`
exports it as `__` so a path begins the way a URL does: `__ / "users" / segment("id", int)`. That is the same argument
that gave the tier `/` -- a path is the one product with a wire syntax of its own -- so a query string and a header set
keep `QNil` and `HNil`, having no such syntax to be named after. The two names are one value and not two code paths:
`__` is an `export` of `Path.Root`, so nothing has to be kept in step.

`*:` reuses every `AppendableOperation` that `:*` does, because the type class names a container and an element rather
than a left and a right. What it gives up is the by-name element: the left operand of a right-associative operator is
the extension parameter, and Scala evaluates it first.

`++` is `zip` written as an operator. It concatenates what the container holds like the other two -- `record ++ record`
writes both sets of fields into one object -- and differs in the Scala value, which stays a pair rather than flattening,
because neither operand is a member of the other. It binds tighter than `:*` and looser than `*:`.

**Neither operator may become `inline`, and neither may the shape evidence they summon.** `Append.Shape` and
`Prepend.Shape` select both the result type and its operations by implicit search, through a ladder of four instances.
That is a measured choice rather than a leftover. Matching on the schema instead copies it into every branch, so each
member roughly doubles the cost of the one before: ten members took forty seconds and thirteen did
not finish. Writing the four instances as one `inline given` over `summonFrom` avoids that and is the only inline
formulation that is even correct -- an `inline match` on `erasedValue` is an error where the scrutinee is neither a
subtype of a pattern nor disjoint from one, and both `Any` and `Nothing` reach it from a member that goes only one way
-- but it was measured 29% slower at forty members and 34% at eighty, and rejected. `Convert.Reader`'s ladder was
measured the same way and rejected for a sharper reason: its union case recurses, so an inline given makes the depth a
union may reach `-Xmax-inlines`, and it fails at 65 where the ladder compiles a hundred. Both scaladocs carry the
numbers. The rule that follows is one line: reach for implicit search here, not for `inline`.

`/` is `:*` restricted to the path tier, in `http`'s `PathSyntax`. It reuses the same `AppendableOperation` instances
rather than adding any, so `segment("users") / segment("id", int)` and `__ :* segment("users") :* segment("id", int)`
are one schema; what it narrows is the element, which has to be a `Segment`, and the result, which has to be a `Path`.
The receiver is left unbounded, because a bound is an upper bound and inferring `Path.Root` against one widens what it
holds from nothing to any segment at all. A literal may be written as a bare `String` -- `__ / "users"` builds exactly
what `segment("users")` builds -- which is the one place a schema is not asked for by name, since a position holding a
fixed piece of text has nothing else to say. Scala reads it left-associatively at the precedence of `*` and `/`, tighter
than `:*` and `++`, which is the direction and the grouping a URL is read with. Two whole paths beside each other is
`++` like anywhere else: `/` puts a segment after a path, as `:*` puts a field beside one.

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
