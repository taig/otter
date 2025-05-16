package io.taig.otter

import cats.Eq
import cats.derived.*
import cats.syntax.all.*
import io.taig.otter.component.JsonKeyComponent.*
import io.taig.otter.codec.JsonKeyParser

final class JsonKeyParserTest extends OtterSuite:
  val decoder = JsonKeyParser

  test("constant"):
    assertEq(
      obtained = decoder.decode(constant("foo"), "foo"),
      expected = ().valid
    )
    assertEq(
      obtained = decoder.decode(constant("foo"), "bar"),
      expected = Violations.rootNec(Violation.equal(reference = "foo", actual = "bar")).invalid
    )

  test("enumeration"):
    enum Animal derives Eq:
      case Bird
      case Cat
      case Dog

    val schema: Json.Key.Enumeration[Animal] = enumeration(string):
      case Animal.Bird => "bird"
      case Animal.Cat  => "cat"
      case Animal.Dog  => "dog"

    assertEq(obtained = decoder.decode(schema, "bird"), expected = Animal.Bird.valid)
    assertEq(obtained = decoder.decode(schema, "cat"), expected = Animal.Cat.valid)
    assertEq(obtained = decoder.decode(schema, "dog"), expected = Animal.Dog.valid)
    assertEq(
      obtained = decoder.decode(schema, "foobar"),
      expected = Violations
        .rootNec(
          Violation.oneOf(
            values = List("bird", "cat", "dog"),
            actual = "foobar"
          )
        )
        .invalid
    )

  test("primitive"):
    assertEq(
      obtained = decoder.decode(string, "foobar"),
      expected = "foobar".valid
    )
    assertEq(
      obtained = decoder.decode(string, ""),
      expected = "".valid
    )

  test("union"):
    val schema = constant("foo") :+ constant("bar")

    assertEq(
      obtained = decoder.decode(schema, "foo"),
      expected = ().asLeft.valid
    )
    assertEq(
      obtained = decoder.decode(schema, "bar"),
      expected = ().asRight.valid
    )
    assertEq(
      obtained = decoder.decode(schema, "foobar"),
      expected = Violations
        .of(
          Step.Index(0) -> Violation.equal(reference = "foo", actual = "foobar"),
          Step.Index(1) -> Violation.equal(reference = "bar", actual = "foobar")
        )
        .invalid
    )
