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
      /** A filter and not a primitive: `Schema.int()` narrows a number rather than naming a different one. */
      test("integrality is a call and not a name"):
        assertTrue(TypescriptEffect.Integral.render == "Schema.int()")
    ),
    suite("combinators")(
      test("a collection is an array, and a non empty one says so"):
        assertTrue(
          TypescriptEffect.array(a).render == "Schema.Array(A)",
          TypescriptEffect.nonEmptyArray(a).render == "Schema.NonEmptyArray(A)"
        )
      ,
      /** An object of more than one member is broken over lines by the printer, so this is what a dictionary really
        * looks like in a generated module.
        */
      test("a dictionary keys itself by string"):
        assertTrue(
          TypescriptEffect.record(a).render ==
            """|Schema.Record({
               |  "key": Schema.String,
               |  "value": A
               |})""".stripMargin
        )
      ,
      test("a tuple and a struct carry what they hold"):
        assertTrue(
          TypescriptEffect.tuple(List(a, b)).render == "Schema.Tuple(A, B)",
          TypescriptEffect.struct(List("x" -> a)).render == """Schema.Struct({ "x": A })"""
        )
      ,
      test("an empty tuple is still a tuple"):
        assertTrue(TypescriptEffect.tuple(Nil).render == "Schema.Tuple()")
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
            .render == """Schema.Literal("a", 1)"""
        )
      ,
      /** A suspension is what a definition refers to itself through, before the constant it names exists. */
      test("a suspension defers to a thunk"):
        assertTrue(TypescriptEffect.suspend(a).render == "Schema.suspend(() => A)")
      ,
      test("a transform names both sides and both directions"):
        assertTrue(
          TypescriptEffect.transform(a, b, a, b).render ==
            """|Schema.transform(
               |  A,
               |  B,
               |  {
               |    "decode": A,
               |    "encode": B
               |  }
               |)""".stripMargin
        )
    ),
    suite("absence")(
      test("nullable, optional, and optional-or-null are three different things"):
        assertTrue(
          TypescriptEffect.nullOr(a).render == "Schema.NullOr(A)",
          TypescriptEffect.optional(a).render == "Schema.optional(A)",
          TypescriptEffect.optionalNullable(a).render == """Schema.optionalWith(A, { "nullable": true })"""
        )
    ),
    suite("union")(
      test("a union of several is a union"):
        assertTrue(TypescriptEffect.union(NonEmptyList.of(a, b)).render == "Schema.Union(A, B)")
      ,
      /** A union of one is that one. Wrapping it would say the same thing at the cost of a level, and every caller
        * builds its members before it knows how many there are.
        */
      test("a union of one is that one"):
        assertTrue(TypescriptEffect.union(NonEmptyList.one(a)).render == "A")
    ),
    suite("filtered")(
      test("a filtered schema is piped through its filters"):
        assertTrue(TypescriptEffect.filtered(a, List(b)).render == "A.pipe(B)")
      ,
      /** Nothing to narrow leaves the schema alone rather than piping it through an empty list, which would not parse.
        */
      test("no filters leaves the schema as it was"):
        assertTrue(TypescriptEffect.filtered(a, Nil).render == "A")
    ),
    suite("types")(
      test("the inferred, encoded and annotated types are three different spellings"):
        val tpe = Typescript.Type.Symbol("Book", Nil)

        assertTrue(
          TypescriptEffect.inferred(tpe).render == "Schema.Schema.Type<Book>",
          TypescriptEffect.encoded(tpe).render == "Schema.Schema.Encoded<Book>",
          TypescriptEffect.annotation(tpe).render == "Schema.Schema<Book>"
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
          rendered.contains("""Schema.Union(Schema.Literal("true"), Schema.Literal("false"))"""),
          rendered.contains(""""decode": (value) => value === "true""""),
          rendered.contains(""""encode": (value) => value ? "true" : "false"""")
        )
      ,
      test("a coerced number accepts a number and the text of one"):
        assertTrue(
          TypescriptEffect.CoerceNumber.render == "Schema.Union(Schema.Number, Schema.NumberFromString)"
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
          rendered.contains(""""decode": (value) => value ? "true" : "false""""),
          rendered.contains(""""encode": (value) => value === "true"""")
        )
    )
  )
