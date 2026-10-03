package io.taig.otter

import cats.data.NonEmptyList
import zio.Scope
import zio.test.*

/** The words the effect `Schema` module is spelled with.
  *
  * Nothing here knows about a schema, so nothing here can be checked by rendering one: this is the target language, and
  * a wrong spelling is source that still prints, still passes every renderer test that only asks what shape came back,
  * and fails only when a person runs `tsc` over it. That is what makes a module of constants worth asserting literally
  * -- the rendered text is the whole of what it promises.
  */
object TypescriptEffectTest extends ZIOSpecDefault:
  private val a: Typescript.Expression = Typescript.Expression.Symbol("A")
  private val b: Typescript.Expression = Typescript.Expression.Symbol("B")

  override def spec: Spec[TestEnvironment & Scope, Any] = suite("TypescriptEffectTest")(
    suite("imports")(
      test("plain schemas import only Schema"):
        assertTrue(
          TypescriptEffect.imports(List(TypescriptEffect.String)).map(_.render) ==
            List("""import { Schema } from "effect";""")
        )
      ,
      test("transformations and optional fields discover all helpers once in stable order"):
        val source = Typescript.Statement.Declaration.Constant(
          exported = true,
          "Example",
          None,
          TypescriptEffect.struct(
            List(
              "number" -> TypescriptEffect.CoerceNumber,
              "optional" -> TypescriptEffect.optionalNullable(TypescriptEffect.String)
            )
          )
        )
        assertTrue(
          TypescriptEffect.imports(List(source, source)).map(_.render) ==
            List("""import { Schema, SchemaTransformation, SchemaGetter, Option } from "effect";""")
        )
      ,
      test("type references count and text containing namespace names does not"):
        val tpe = Typescript.Statement.Declaration.Type(
          exported = true,
          "Example",
          TypescriptEffect.encoded(Typescript.Type.TypeOf(a))
        )
        assertTrue(
          TypescriptEffect.imports(List(tpe)).map(_.render) == List("""import { Schema } from "effect";"""),
          TypescriptEffect.imports(List(Typescript.Expression.Literal.String("SchemaGetter.transform"))).isEmpty,
          TypescriptEffect.imports(Nil).isEmpty
        )
    ),
    suite("naming")(
      test("a member is reached through the module"):
        assertTrue(TypescriptEffect.symbol("Foo").render == "Schema.Foo", TypescriptEffect(a).render == "Schema.A")
      ,
      test("the primitives name themselves"):
        assertTrue(
          TypescriptEffect.Boolean.render == "Schema.Boolean",
          TypescriptEffect.Int.render == "Schema.Int",
          TypescriptEffect.Null.render == "Schema.Null",
          TypescriptEffect.Number.render == "Schema.Number",
          TypescriptEffect.String.render == "Schema.String"
        )
      ,
      /** A filter and not a primitive: `Schema.isInt()` narrows a number rather than naming a different one. */
      test("integrality is a call and not a name"):
        assertTrue(TypescriptEffect.Integral.render == "Schema.isInt()")
    ),
    suite("combinators")(
      test("a collection is an array, and a non empty one says so"):
        assertTrue(
          TypescriptEffect.array(a).render == "Schema.Array(A)",
          TypescriptEffect.nonEmptyArray(a).render == "Schema.NonEmptyArray(A)"
        )
      ,
      test("a dictionary keys itself by string"):
        assertTrue(
          TypescriptEffect.record(a).render ==
            "Schema.Record(Schema.String, A)"
        )
      ,
      test("a tuple and a struct carry what they hold"):
        assertTrue(
          TypescriptEffect.tuple(List(a, b)).render == """Schema.Tuple(
                                                         |  [
                                                         |    A,
                                                         |    B
                                                         |  ]
                                                         |)""".stripMargin,
          TypescriptEffect.struct(List("x" -> a)).render == """Schema.Struct({ "x": A })"""
        )
      ,
      test("an empty tuple is still a tuple"):
        assertTrue(TypescriptEffect.tuple(Nil).render == "Schema.Tuple([])")
      ,
      test("a literal carries every value it was given"):
        assertTrue(
          TypescriptEffect
            .literal(
              NonEmptyList.of(
                Typescript.Expression.Literal.String("a"),
                Typescript.Expression.Literal.Number(java.math.BigDecimal.valueOf(1L))
              )
            )
            .render == """Schema.Literals(
                         |  [
                         |    "a",
                         |    1
                         |  ]
                         |)""".stripMargin
        )
      ,
      /** A suspension is what a definition refers to itself through, before the constant it names exists. */
      test("a suspension defers to a thunk"):
        assertTrue(TypescriptEffect.suspend(a).render == "Schema.suspend(() => A)")
      ,
      test("a transform names both sides and both directions"):
        assertTrue(
          TypescriptEffect.transform(a, b, a, b).render ==
            """A.pipe(
              |  Schema.decodeTo(
              |    B,
              |    SchemaTransformation.transform({
              |      "decode": A,
              |      "encode": B
              |    })
              |  )
              |)""".stripMargin
        )
    ),
    suite("absence")(
      test("nullable, optional, and optional-or-null are three different things"):
        assertTrue(
          TypescriptEffect.nullOr(a).render == "Schema.NullOr(A)",
          TypescriptEffect.optional(a).render == "Schema.optional(A)",
          TypescriptEffect
            .optionalNullable(a)
            .render == """Schema.optional(Schema.NullOr(A)).pipe(
                         |  Schema.decodeTo(
                         |    Schema.optional(Schema.toType(A)),
                         |    {
                         |      "decode": SchemaGetter.transformOptional(Option.filter((value) => (value !== null))),
                         |      "encode": SchemaGetter.transformOptional((value) => value)
                         |    }
                         |  )
                         |)""".stripMargin
        )
    ),
    suite("union")(
      test("a union of several is a union"):
        assertTrue(TypescriptEffect.union(NonEmptyList.of(a, b)).render == """Schema.Union(
                                                                             |  [
                                                                             |    A,
                                                                             |    B
                                                                             |  ]
                                                                             |)""".stripMargin)
      ,
      /** A union of one is that one. Wrapping it would say the same thing at the cost of a level, and every caller
        * builds its members before it knows how many there are.
        */
      test("a union of one is that one"):
        assertTrue(TypescriptEffect.union(NonEmptyList.one(a)).render == "A")
    ),
    suite("filtered")(
      test("a filtered schema applies its checks"):
        assertTrue(TypescriptEffect.filtered(a, List(b)).render == "A.check(B)")
      ,
      /** Nothing to narrow leaves the schema alone. */
      test("no filters leaves the schema as it was"):
        assertTrue(TypescriptEffect.filtered(a, Nil).render == "A")
    ),
    suite("types")(
      test("the inferred, encoded and annotated types are three different spellings"):
        val tpe = Typescript.Type.Symbol("Book", Nil)

        assertTrue(
          TypescriptEffect.inferred(tpe).render == "Schema.Schema.Type<Book>",
          TypescriptEffect.encoded(tpe).render == "Schema.Codec.Encoded<Book>",
          TypescriptEffect.annotation(tpe).render == "Schema.Codec<Book, Book>"
        )
      ,
      test("annotations retain both projections for transformed and symmetric schemas"):
        val decoded = Typescript.Type.Symbol("Book", Nil)
        val encoded = Typescript.Type.Symbol("BookEncoded", Nil)

        assertTrue(
          TypescriptEffect.annotation(decoded, encoded).render == "Schema.Codec<Book, BookEncoded>",
          TypescriptEffect.annotation(decoded, decoded).render == "Schema.Codec<Book, Book>"
        )
      ,
      /** What decides whether a constant needs an ascription: a type it inferred from its own value does not. */
      test("only an inferred type reads as inferred"):
        val tpe = Typescript.Type.Symbol("Book", Nil)

        assertTrue(
          TypescriptEffect.isInferred(TypescriptEffect.inferred(tpe)),
          !TypescriptEffect.isInferred(TypescriptEffect.encoded(tpe)),
          !TypescriptEffect.isInferred(TypescriptEffect.annotation(tpe)),
          !TypescriptEffect.isInferred(tpe)
        )
    ),
    /** The laxer wire forms a coerced value accepts. These have to match what the decoders normalise, which is the one
      * claim about them that a renderer test cannot make: it would only be comparing the generator with itself.
      */
    suite("coercion")(
      test("a coerced boolean accepts a boolean and the two strings that spell one"):
        val rendered = TypescriptEffect.CoerceBoolean.render

        assertTrue(
          rendered.startsWith("Schema.Union("),
          rendered.contains("Schema.Boolean"),
          rendered.contains("Schema.Literal(\"true\")"),
          rendered.contains("Schema.Literal(\"false\")"),
          rendered.contains(""""decode": (value) => value === "true""""),
          rendered.contains(""""encode": (value) => value ? "true" : "false"""")
        )
      ,
      test("a coerced number accepts a number and the text of one"):
        assertTrue(
          TypescriptEffect.CoerceNumber.render.contains("Schema.isPattern(RegExp("),
          TypescriptEffect.CoerceNumber.render.contains("\"decode\": (value) => Number(value)"),
          TypescriptEffect.CoerceNumber.render.contains("\"encode\": (value) => String(value)")
        )
      ,
      /** Both directions of both transforms, because a coercion that decoded correctly and encoded the other way round
        * would still be a schema effect accepts.
        */
      test("a coerced string accepts a string, a number and a boolean"):
        val rendered = TypescriptEffect.CoerceString.render

        assertTrue(
          rendered.startsWith("Schema.Union("),
          rendered.contains("Schema.String"),
          rendered.contains(""""decode": (value) => String(value)"""),
          rendered.contains(""""encode": (value) => Number(value)"""),
          rendered
            .sliding("\"decode\": (value) => String(value)".length)
            .count(_ == "\"decode\": (value) => String(value)") == 2,
          rendered.contains(""""encode": (value) => value === "true"""")
        )
    )
  )
