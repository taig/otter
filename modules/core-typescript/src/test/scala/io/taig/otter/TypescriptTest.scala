package io.taig.otter

import zio.test.ZIOSpecDefault
import zio.Scope
import zio.test.*

object TypescriptTest extends ZIOSpecDefault:
  override def spec: Spec[TestEnvironment & Scope, Any] = suite("TypescriptTest")(
    test("render"):
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
    test("render: object (empty)"):
      val expression = Typescript.Expression.Object(fields = Nil)
      assertTrue(expression.render == "{}")
    ,
    test("render: object (single)"):
      val expression = Typescript.Expression.Object(fields = List("foo" -> Typescript.Expression.Identifier("bar")))
      assertTrue(expression.render == "{ \"foo\": bar }")
  )
