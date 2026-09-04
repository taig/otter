# Otter

Extensible schema definition library for serialization formats (e.g. JSON, XML and CSV) with self-documenting API definition capabilities.

## Development workflow

Modules: `core`, `core-json`, `core-json-borer`, `core-json-circe`, `core-json-schema`, `core-csv`,
`core-csv-fs2-data`, `core-iron`, `core-java-time`, `core-case-insensitive`, `core-typescript`,
`core-typescript-effect`, `core-json-typescript`, `core-json-typescript-effect`, `http`, `http-json`,
`http-openapi`.
Each cross builds to the JVM and Scala.js; the Scala.js project ids carry a `JS` suffix
(`core-json-circeJS`).

`benchmark` is apart from all of them: JVM only, published nowhere, and not run by `testJVM`. It holds the JMH
benchmarks that say where a read and a write actually spend their time -- `sbt "benchmark/Jmh/run -wi 3 -i 5 -f 1"`,
and add `-prof gc` for `gc.alloc.rate.norm`, which is deterministic and so says more than a timing does. It measures
the `core-json-circe` fixtures, which is why it depends on that module's test sources.

How to read a result. On **reads**, `parseText` is the document model on its own. On **writes** there are two halves
and neither `encodeDocument` nor `printDocument` is one of them: `encodeDocument` walks the schema *and* builds the
tree, so `encodeVoid` -- the same walk with nothing built, via `JsonVoidEncoder` -- is what you subtract.
`(encodeDocument - encodeVoid) + printDocument` is the document model's share of a write.

Measured, that share is **64% of a flat record's write, 79% of a nested document's and ~95% of a small one**. The
schema interpreter is the other 14-29% of a write and allocates almost nothing (24B to 1.2KB per op, against 5.4KB to
39KB for the whole write). That asymmetry is why `core-json-borer` exists and why its encoder carries a deferred write
rather than a document.

Reads are the other way round and used to be worse: the interpreter was 72-90% of a read and allocated 0.9-3.4KB *per
node*. It is now 493ns and 3.3KB for a fifteen field record where it was 2070ns and 16.2KB, so parsing is 21% to 53% of
a read rather than 10% to 28%, and for a small record the read is essentially the parse.

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