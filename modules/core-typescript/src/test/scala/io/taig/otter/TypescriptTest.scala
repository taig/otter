package io.taig.otter

import cats.data.NonEmptyList
import zio.Scope
import zio.test.*

/** The printer decides layout from whether a child already broke, and from nothing else. What that has to guarantee is
  * that a short thing stays short and a long or already broken thing does not run off the line.
  */
object TypescriptTest extends ZIOSpecDefault:
  private val Schema: String = "Schema"

  private def call(name: String, arguments: Typescript.Expression*): Typescript.Expression =
    Typescript.Expression.Member(TypescriptTest.Schema, Typescript.Expression.Call(name, arguments.toList))

  private def symbol(name: String): Typescript.Expression =
    Typescript.Expression.Member(TypescriptTest.Schema, Typescript.Expression.Symbol(name))

  private def tpe(name: String): Typescript.Type = Typescript.Type.Symbol(name, parameters = Nil)

  override def spec: Spec[TestEnvironment & Scope, Any] = suite("TypescriptTest")(
    suite("expression")(
      test("a call with no arguments"):
        assertTrue(call("Struct").render == "Schema.Struct()")
      ,
      test("a call with one short argument stays on the line"):
        assertTrue(call("Array", symbol("String")).render == "Schema.Array(Schema.String)")
      ,
      /** An object literal is already delimited, so breaking around it would only add a line that says nothing. */
      test("a call wrapping an object keeps the brace on the line"):
        val expression = call(
          "Struct",
          Typescript.Expression.Object(List("first" -> symbol("String"), "last" -> symbol("String")))
        )

        assertTrue(expression.render == """Schema.Struct({
                                          |  first: Schema.String,
                                          |  last: Schema.String
                                          |})""".stripMargin)
      ,
      test("a call with several short arguments stays on the line"):
        val expression = call("Literal", literal("bird"), literal("cat"), literal("dog"))
        assertTrue(expression.render == """Schema.Literal("bird", "cat", "dog")""")
      ,
      test("a call breaks once its arguments no longer fit"):
        val expression = call("Literal", literal("bird" * 20), literal("cat" * 20))

        assertTrue(expression.render == s"""Schema.Literal(
                                           |  "${"bird" * 20}",
                                           |  "${"cat" * 20}"
                                           |)""".stripMargin)
      ,
      test("a call breaks as soon as one argument has broken"):
        val expression = call(
          "Union",
          symbol("String"),
          call("Struct", Typescript.Expression.Object(List("a" -> symbol("String"), "b" -> symbol("String"))))
        )

        assertTrue(expression.render == """Schema.Union(
                                          |  Schema.String,
                                          |  Schema.Struct({
                                          |    a: Schema.String,
                                          |    b: Schema.String
                                          |  })
                                          |)""".stripMargin)
      ,
      test("an object with one short field stays on the line"):
        assertTrue(
          Typescript.Expression.Object(List("nullable" -> Typescript.Expression.Literal.Boolean(true))).render ==
            """{ nullable: true }"""
        )
      ,
      /** A key that is already a name is written as one; a key that is only a key when quoted keeps its quotes. A JSON
        * document may be keyed by anything, and the generated source has to say what the schema said.
        */
      test("a key is quoted only where it has to be"):
        def obj(name: String): String =
          Typescript.Expression.Object(List(name -> Typescript.Expression.Literal.Boolean(true))).render

        assertTrue(
          obj("nullable") == "{ nullable: true }",
          obj("$ref") == "{ $ref: true }",
          obj("_private") == "{ _private: true }",
          obj("accept-language") == """{ "accept-language": true }""",
          obj("two words") == """{ "two words": true }""",
          obj("2fa") == """{ "2fa": true }""",
          obj("") == """{ "": true }"""
        )
      ,
      test("an array, empty, short and broken"):
        val long = symbol("SomethingWithAVeryLongNameIndeedYesReally")

        assertTrue(
          Typescript.Expression.Array(Nil).render == "[]",
          Typescript.Expression.Array(List(symbol("String"))).render == "[Schema.String]",
          Typescript.Expression.Array(List(long, long)).render == s"""[
                                                                     |  $long,
                                                                     |  $long
                                                                     |]""".stripMargin
        )
      ,
      test("an empty object"):
        assertTrue(Typescript.Expression.Object(Nil).render == "{}")
      ,
      test("a loose comparison"):
        val value = Typescript.Expression.Symbol("value")
        assertTrue(Typescript.Expression.Equal(value, literal("true")).render == """value == "true"""")
      ,
      /** [[Typescript.Expression.Member]] reaches a name; a pipe reaches a property of a value. */
      test("a pipe refines a value"):
        val expression = Typescript.Expression.Pipe(
          symbol("String"),
          NonEmptyList.of(call("minLength", Typescript.Expression.Literal.Number(new java.math.BigDecimal(3))))
        )

        assertTrue(expression.render == "Schema.String.pipe(Schema.minLength(3))")
      ,
      test("a pipe breaks when what it refines has broken"):
        val expression = Typescript.Expression.Pipe(
          call("Struct", Typescript.Expression.Object(List("a" -> symbol("String"), "b" -> symbol("String")))),
          NonEmptyList.of(call("filter", symbol("String")))
        )

        assertTrue(expression.render == """Schema.Struct({
                                          |  a: Schema.String,
                                          |  b: Schema.String
                                          |}).pipe(
                                          |  Schema.filter(Schema.String)
                                          |)""".stripMargin)
      ,
      test("an arrow, a ternary and a strict comparison"):
        val value = Typescript.Expression.Symbol("value")

        val expression = Typescript.Expression.Arrow(
          List(value),
          Typescript.Expression.Ternary(
            Typescript.Expression.TripleEqual(value, literal("true")),
            literal("yes"),
            literal("no")
          )
        )

        assertTrue(expression.render == """(value) => value === "true" ? "yes" : "no"""")
    ),
    suite("statement")(
      test("a constant, exported and not, with and without a type"):
        val value = symbol("String")

        assertTrue(
          Typescript.Statement.Declaration.Constant(true, "A", none, value).render == "export const A = Schema.String;",
          Typescript.Statement.Declaration.Constant(false, "A", none, value).render == "const A = Schema.String;",
          Typescript.Statement.Declaration
            .Constant(true, "A", Some(tpe("Foo")), value)
            .render == "export const A: Foo = Schema.String;"
        )
      ,
      test("a variable, exported and not, with and without a type"):
        val value = symbol("String")

        assertTrue(
          Typescript.Statement.Declaration.Variable(true, "A", none, value).render == "export let A = Schema.String;",
          Typescript.Statement.Declaration.Variable(false, "A", none, value).render == "let A = Schema.String;",
          Typescript.Statement.Declaration
            .Variable(true, "A", Some(tpe("Foo")), value)
            .render == "export let A: Foo = Schema.String;"
        )
      ,
      test("a block of evaluated expressions"):
        val block = Typescript.Statement.Block(
          List(
            Typescript.Statement.Evaluate(call("Array", symbol("String"))),
            Typescript.Statement.Declaration.Constant(false, "A", none, symbol("Int"))
          )
        )

        assertTrue(block.render == """{
                                     |  Schema.Array(Schema.String);
                                     |  const A = Schema.Int;
                                     |}""".stripMargin)
      ,
      test("a type declaration"):
        assertTrue(
          Typescript.Statement.Declaration.Type(true, "A", tpe("string")).render == "export type A = string;",
          Typescript.Statement.Declaration.Type(false, "A", tpe("string")).render == "type A = string;"
        )
    ),
    suite("type")(
      test("an object names its members and terminates each"):
        val value = Typescript.Type.Object(
          List(
            Typescript.Type.Field("title", tpe("string"), optional = false),
            Typescript.Type.Field("tag", tpe("number"), optional = true)
          )
        )

        assertTrue(value.render == """{
                                     |  title: string;
                                     |  tag?: number | undefined;
                                     |}""".stripMargin)
      ,
      /** A single member is short enough to read inline, and then the separator would be the last thing on the line. */
      test("an object with one member needs no separator"):
        val value = Typescript.Type.Object(List(Typescript.Type.Field("tag", tpe("number"), optional = false)))
        assertTrue(value.render == """{ tag: number }""")
      ,
      test("a short union stays on the line"):
        val value = Typescript.Type.Union(NonEmptyList.of(tpe("number"), Typescript.Type.Null))
        assertTrue(value.render == "number | null")
      ,
      test("a union whose members have broken puts each on its own line"):
        val member = Typescript.Type.Object(
          List(
            Typescript.Type.Field("base", tpe("number"), optional = false),
            Typescript.Type.Field("height", tpe("number"), optional = false)
          )
        )

        val value = Typescript.Type.Union(
          NonEmptyList.of(Typescript.Type.Object(List(Typescript.Type.Field("side", tpe("number"), false))), member)
        )

        assertTrue(value.render == """#| { side: number }
                                      #| {
                                      #    base: number;
                                      #    height: number;
                                      #  }""".stripMargin('#'))
      ,
      test("a parameterised symbol, a tuple and a typeof"):
        assertTrue(
          Typescript.Type.Symbol("ReadonlyArray", List(tpe("number"))).render == "ReadonlyArray<number>",
          Typescript.Type.Tuple(List(tpe("string"), tpe("number"))).render == "[string, number]",
          Typescript.Type.Tuple(Nil).render == "[]",
          Typescript.Type.TypeOf(Typescript.Expression.Symbol("Book")).render == "typeof Book"
        )
    )
  )

  private def literal(value: String): Typescript.Expression.Literal = Typescript.Expression.Literal.String(value)

  private def none: Option[Typescript.Type] = Option.empty
