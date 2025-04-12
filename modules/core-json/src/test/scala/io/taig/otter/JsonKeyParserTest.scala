package io.taig.otter

import cats.Eq
import cats.derived.*
import cats.syntax.all.*
import io.taig.otter.JsonDsl.key.*

final class JsonKeyParserTest extends OtterSuite:
  val parser = JsonKeyParser

  test("constant"):
    assertEq(
      obtained = parser(constant("foo"), "foo"),
      expected = "foo".valid
    )
    assertEq(
      obtained = parser(constant("foo"), "bar"),
      expected = Violations.rootNec(Violation.equal(reference = "foo", actual = "bar")).invalid
    )

  test("enumeration"):
    enum Animal derives Eq:
      case Bird
      case Cat
      case Dog

    val codec: Json.Key.Enumeration[Animal] = enumeration(string):
      case Animal.Bird => "bird"
      case Animal.Cat  => "cat"
      case Animal.Dog  => "dog"

    assertEq(obtained = parser(codec, "bird"), expected = Animal.Bird.valid)
    assertEq(obtained = parser(codec, "cat"), expected = Animal.Cat.valid)
    assertEq(obtained = parser(codec, "dog"), expected = Animal.Dog.valid)
    assertEq(
      obtained = parser(codec, "foobar"),
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
      obtained = parser(string, "foobar"),
      expected = "foobar".valid
    )
    assertEq(
      obtained = parser(string, ""),
      expected = "".valid
    )

  // test("union"):
  //   val codec = branch("x", constant("foo")) :+ branch("y", constant("bar"))

  //   assertEq(
  //     obtained = parser(codec, "foo"),
  //     expected = "foo".asLeft.valid
  //   )
  //   assertEq(
  //     obtained = parser(codec, "bar"),
  //     expected = "bar".asRight.valid
  //   )
  //   assertEq(
  //     obtained = parser(codec, "foobar"),
  //     expected = Violations
  //       .of(
  //         Step.Field("x") -> Violation.equal(reference = "foo", actual = "foobar"),
  //         Step.Field("y") -> Violation.equal(reference = "bar", actual = "foobar")
  //       )
  //       .invalid
  //   )
