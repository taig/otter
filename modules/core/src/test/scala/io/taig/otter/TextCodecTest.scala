package io.taig.otter

import io.taig.otter.codec.TextParser
import io.taig.otter.codec.TextPrinter
import io.taig.otter.component.TextComponent.*
import zio.*
import zio.test.*

object TextCodecTest extends ZIOSpecDefault:
  enum Animal:
    case Bird, Cat, Dog

  override def spec: Spec[TestEnvironment & Scope, Any] = suite("TextCodecTest")(
    test("primitive"):
      val input = TextPrinter.print(string, "foobar")
      val result = TextParser.parse(string, input)

      assertTrue(result.toEither.is(_.right) == input)
    ,
    test("constant"):
      val schema = constant(string, "foo")
      val input = TextPrinter.print(schema, "bar")
      val result = TextParser.parse(schema, input)

      assertTrue(
        input == "foo",
        result.toEither.is(_.right) == input
      )
    ,
    test("enumeration"):
      val schema = enumeration[Animal](string):
        case Animal.Bird => "bird"
        case Animal.Cat  => "cat"
        case Animal.Dog  => "dog"
      val input = TextPrinter.print(schema, Animal.Cat)
      val result = TextParser.parse(schema, input)

      assertTrue(result.toEither.is(_.right) == Animal.Cat)
  )
