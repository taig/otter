package io.taig.openapi

import munit.FunSuite

final class HistoryTest extends FunSuite:
  test("toJsonPath") {
    val path = History.Root / "baz" / 3 / "bar" / "foo"
    assertEquals(obtained = path.toJsonPath, expected = ".baz[3].bar.foo")
  }

  test("toJsonPath: root") {
    assertEquals(obtained = History.Root.toJsonPath, expected = ".")
  }

  test("toJsonPath: array") {
    val path = History.Root / 3 / "foo"
    assertEquals(obtained = path.toJsonPath, expected = "[3].foo")
  }

  test("toList") {
    val path = History.Root / "baz" / 3 / "bar"
    assertEquals(
      obtained = path.toList,
      expected = List(History.Step.Field("baz"), History.Step.Index(3), History.Step.Field("bar"))
    )
  }

  test("up") {
    val path = History.Root / "baz" / 3 / "bar" / "foo"
    assertEquals(obtained = path.up, expected = History.Root / "baz" / 3 / "bar")
  }

  test("parse") {
    assertEquals(
      obtained = History.parse(".foo.bar[3].baz"),
      expected = Right(History.Root / "foo" / "bar" / 3 / "baz")
    )
  }

  test("parse: root") {
    assertEquals(
      obtained = History.parse("."),
      expected = Right(History.Root)
    )
  }

  test("parse: array") {
    assertEquals(
      obtained = History.parse("[3].foo"),
      expected = Right(History.Root / 3 / "foo")
    )
  }

  test("parse: empty") {
    assertEquals(
      obtained = History.parse(""),
      expected = Left("Empty")
    )
  }

  test("parse: invalid characters") {
    assertEquals(
      obtained = History.parse(".foo I don't belong here .bar[3].baz"),
      expected = Left("Contains invalid characters")
    )
  }

  test("parse: exhaustive") {
    assertEquals(
      obtained = History.parse(".foo.bar[3].baz I don't belong here"),
      expected = Left("Contains invalid characters")
    )
  }
