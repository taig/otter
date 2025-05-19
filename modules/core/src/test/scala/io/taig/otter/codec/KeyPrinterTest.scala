package io.taig.otter.codec

import cats.syntax.all.*
import io.taig.otter.Key
import io.taig.otter.OtterSuite
import io.taig.otter.component.KeyComponent.*

final class JsonKeyPrinterTest extends OtterSuite:
  val encoder = KeyPrinter

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

    val schema: Key.Enumeration[Animal] = enumeration(string):
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
    val schema = constant("foo") +: constant("bar")

    assertEq(
      obtained = encoder.encode(schema, ().asLeft),
      expected = "foo"
    )
    assertEq(
      obtained = encoder.encode(schema, ().asRight),
      expected = "bar"
    )
