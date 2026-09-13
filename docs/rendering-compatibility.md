# Rendering compatibility

Renderers describe wire values. They do not run arbitrary Scala transformations or validate values passed to an encoder.

JSON Schema preserves repeated constraints as conjunctions. `Int` and `Long` carry their numeric bounds when the profile permits constraints. A profile that omits these bounds reports them through `JsonSchemaIssue.Dropped`. Numeric coercion describes the JSON-number grammar for strings; numeric bounds and other numeric keywords cannot constrain the value represented by a string, so Scala validation is still necessary for that branch.

Scala text lengths count UTF-16 code units. Effect uses the same count, but JSON Schema counts code points. JSON Schema therefore omits text-length constraints and reports `Dropped`, rather than rejecting valid Scala strings containing supplementary characters.

Constraint regexes use a conservative, unflagged subset of Java and ECMA-262: ASCII literals, ordinary character classes, grouping, alternation, quantifiers, and selected escapes. Matches consume the whole input, including its final line terminator. Compiled flags, backreferences, class intersections, Unicode properties and other unsupported constructs are omitted. JSON Schema reports the omitted constraint. The Effect renderer retains its source-only API and provides no omission diagnostics; generated Effect schemas must not be treated as complete replacements for Scala validation.

Generated Effect `Int` schemas enforce the signed 32-bit range. Coerced strings use JSON number syntax, so `"1.0"` and `"1e2"` can represent integers, while `"0x10"`, `"+1"`, `"01"` and surrounding whitespace are rejected. The decimal text is checked for integrality and range before conversion to a JavaScript number. This follows borer’s coercion grammar. Circe additionally accepts some non-JSON spellings, including `"01"` and `".1"`; generated schemas deliberately exclude these. Backend-specific parser limits on exponents and document sizes are not modeled. Existing number values have already been rounded by JavaScript; the renderer cannot recover their original JSON lexemes.

Effect `Long` schemas constrain the representable numbers to `[-2^63, 2^63)`. The upper bound is exclusive because JavaScript rounds `Long.MaxValue` to `2^63`. This is not lossless `Long` support. `Long`, arbitrary-precision numbers, and floating-point conversions remain subject to JavaScript number precision and range. Use explicit target overrides when an application needs a different representation.
