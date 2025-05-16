package io.taig.otter

import io.taig.otter.component.JsonKeyComponent.*
import io.taig.otter.codec.JsonKeyPrinter
import cats.syntax.all.*

final class JsonKeyPrinterTest extends OtterSuite:
  val encoder = JsonKeyPrinter

  test("constant"):
    assertEq(
      obtained = encoder.encode(constant("foo"), ()),
      expected = "foo"
    )

  test("enumeration"):
    enum Animal:
      case Bird
      case Cat
      case Dog

    val schema: Json.Key.Enumeration[Animal] = enumeration(string):
      case Animal.Bird => "bird"
      case Animal.Cat  => "cat"
      case Animal.Dog  => "dog"

    assertEq(
      obtained = encoder.encode(schema, Animal.Bird),
      expected = "bird"
    )
    assertEq(
      obtained = encoder.encode(schema, Animal.Cat),
      expected = "cat"
    )
    assertEq(
      obtained = encoder.encode(schema, Animal.Dog),
      expected = "dog"
    )

  test("primitive"):
    assertEq(
      obtained = encoder.encode(string, "foobar"),
      expected = "foobar"
    )
    assertEq(
      obtained = encoder.encode(string, ""),
      expected = ""
    )

  test("union"):
    val schema = constant("foo") :+ constant("bar")

    assertEq(
      obtained = encoder.encode(schema, ().asLeft),
      expected = "foo"
    )
    assertEq(
      obtained = encoder.encode(schema, ().asRight),
      expected = "bar"
    )
