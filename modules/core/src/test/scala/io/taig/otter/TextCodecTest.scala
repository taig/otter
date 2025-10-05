package io.taig.otter

import munit.FunSuite
import io.taig.otter.component.TextComponent.*
import io.taig.otter.codec.TextPrinter
import io.taig.otter.codec.TextParser
import zio.test.ZIOSpecDefault
import zio.*
import zio.test.*
import zio.test.Assertion.*

object TextCodecTest extends ZIOSpecDefault:
  override def spec: Spec[TestEnvironment & Scope, Any] = suite("TextCodecTest")(
    test("primitive"):
      val input = TextPrinter.print(string, "foobar")
      val result = TextParser.parse(string, input)

      assertTrue(result.toEither.is(_.right) == input),
    test("constant"):
      val schema = constant(string, "foo")
      val input = TextPrinter.print(schema, "bar")
      val result = TextParser.parse(schema, input)

      assertTrue(
        input == "foo",
        result.toEither.is(_.right) == input
      )
  )