package io.taig.otter

import zio.Scope
import zio.test.*

object TypescriptControlTest extends ZIOSpecDefault:
  override val spec: Spec[TestEnvironment & Scope, Any] = suite("TypescriptControlTest")(
    test("binary expressions retain grouping inside a returned expression"):
      val a = Typescript.Expression.Symbol("a")
      val b = Typescript.Expression.Symbol("b")
      val c = Typescript.Expression.Symbol("c")
      val body = Typescript.Expression.Binary(
        Typescript.Expression.Binary(a, Typescript.Expression.Operator.Add, b),
        Typescript.Expression.Operator.Subtract,
        c
      )
      val function =
        Typescript.Expression.Arrow(List(a, b, c), Typescript.Statement.Block(List(Typescript.Statement.Return(body))))
      assertTrue(function.render == "(a, b, c) => {\n  return ((a + b) - c);\n}")
  )
