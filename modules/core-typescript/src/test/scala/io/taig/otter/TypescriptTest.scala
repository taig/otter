package io.taig.otter

import zio.test.ZIOSpecDefault
import zio.Scope
import zio.test.*

object TypescriptTest extends ZIOSpecDefault:
  override def spec: Spec[TestEnvironment & Scope, Any] = suite("TypescriptTest")(
    test("render: Expression.Array"):
      val expression = Typescript.Expression.Array(
        elements = List(
          Typescript.Expression.Identifier("bar"),
          Typescript.Expression.Member(
            namespace = "foo",
            property = Typescript.Expression.Identifier("bar")
          )
        )
      )

      val expected = """[
                       |  bar,
                       |  foo.bar
                       |]""".stripMargin

      assertTrue(expression.render == expected)
    ,
    test("render: Expression.Array (empty)"):
      val expression = Typescript.Expression.Array(Nil)
      assertTrue(expression.render == "[]")
    ,
    test("render: Expression.Array (single)"):
      val expression = Typescript.Expression.Array(
        elements = List(Typescript.Expression.Identifier(name = "foo"))
      )
      assertTrue(expression.render == "[foo]")
    ,
    test("render: Expression.Object"):
      val expression = Typescript.Expression.Object(
        fields = List(
          "foo" -> Typescript.Expression.Identifier("bar"),
          "bar" -> Typescript.Expression.Member(
            namespace = "foo",
            property = Typescript.Expression.Identifier("bar")
          )
        )
      )

      val expected = """{
                       |  "foo": bar,
                       |  "bar": foo.bar
                       |}""".stripMargin

      assertTrue(expression.render == expected)
    ,
    test("render: Expression.Object (empty)"):
      val expression = Typescript.Expression.Object(fields = Nil)
      assertTrue(expression.render == "{}")
    ,
    test("render: Expression.Object (single)"):
      val expression = Typescript.Expression.Object(fields = List("foo" -> Typescript.Expression.Identifier("bar")))
      assertTrue(expression.render == "{ \"foo\": bar }")
  )
