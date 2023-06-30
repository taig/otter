package io.taig.openapi.http

import cats.data.Chain
import cats.syntax.all.*
import io.taig.openapi.{History, OpenApi}
import io.taig.openapi.http.syntax.*
import io.taig.openapi.syntax.*
import io.taig.openapi.schema.Violations
import io.taig.openapi.schema.schemas.*
import io.taig.openapi.validation.Constraint
import munit.FunSuite

final class PathTest extends FunSuite:
  val x: Segment[String] = parameter("x", string)
  val y: Segment[Int] = parameter("y", int)
  val z: Segment[Long] = parameter("z", long)
  val path: Path[(String, Int, Long)] = Path.Root / x / "foo" / y / "bar" / z

  test("toChain") {
    assertEquals(obtained = path.toChain, expected = Chain(x, Segment.Static("foo"), y, Segment.Static("bar"), z))
  }

  test("matches") {
    assertEquals(
      obtained = path.matches(Chain("a", "foo", "42", "bar", "3")),
      expected = true
    )
    assertEquals(
      obtained = path.matches(Chain("a", "foo", "42", "bar", "3", "x")),
      expected = false
    )
    assertEquals(
      obtained = path.matches(Chain("a", "foo", "42", "bar")),
      expected = false
    )
    assertEquals(
      obtained = path.matches(Chain("foo", "42", "bar", "3")),
      expected = false
    )
    assertEquals(
      obtained = path.matches(Chain("a", "oof", "42", "rab", "3")),
      expected = false
    )
  }

  test("matches: Path.Root") {
    assertEquals(
      obtained = Path.Root.matches(Chain.empty),
      expected = true
    )
    assertEquals(
      obtained = Path.Root.matches(Chain("foo")),
      expected = false
    )
  }

  test("matchesWithRemainders") {
    assertEquals(
      obtained = path.matchesWithRemainders(Chain("a", "foo", "42", "bar", "3")),
      expected = (Chain.empty, true)
    )
    assertEquals(
      obtained = path.matchesWithRemainders(Chain("a", "foo", "42", "bar", "3", "x")),
      expected = (Chain("x"), true)
    )
    assertEquals(
      obtained = path.matchesWithRemainders(Chain("a", "foo", "42", "bar")),
      expected = (Chain.empty, false)
    )
    assertEquals(
      obtained = path.matchesWithRemainders(Chain("foo", "42", "bar", "3")),
      expected = (Chain.empty, false)
    )
    assertEquals(
      obtained = path.matchesWithRemainders(Chain("a", "oof", "42", "rab", "3")),
      expected = (Chain.empty, false)
    )
  }

  test("matchesWithRemainders: Path.Root") {
    assertEquals(
      obtained = Path.Root.matchesWithRemainders(Chain.empty),
      expected = (Chain.empty, true)
    )
    assertEquals(
      obtained = Path.Root.matchesWithRemainders(Chain("foo")),
      expected = (Chain("foo"), true)
    )
  }

  test("decode") {
    assertEquals(
      obtained = path.decode(Chain("foobar", "foo", "42", "bar", "3")),
      expected = ("foobar", 42, 3L).valid
    )
    assertEquals(
      obtained = path.decode(Chain.empty),
      expected = Violations.oneNec(History.Root / "x", Constraint.required.toViolation(OpenApi.Null)).invalid
    )
    assertEquals(
      obtained = path.decode(Chain("foobar", "foo", "42", "bar", "3", "?")),
      expected = Violations.rootNec(Constraint.text.equal("/".asOpenApi).toViolation("/?".asOpenApi)).invalid
    )
    assertEquals(
      obtained = path.decode(Chain("foobar", "foo", "NaN", "bar", "3")),
      expected = Violations
        .oneNec(History.Root / "y", Constraint.tpe("OpenApi.Int".asOpenApi).toViolation("NaN".asOpenApi))
        .invalid
    )
    assertEquals(
      obtained = path.decode(Chain("?", "foobar", "foo", "42", "bar", "3")),
      expected = Violations.oneNec(History.Root / "foo", Constraint.required.toViolation("foobar".asOpenApi)).invalid
    )
  }

  test("decodeWithRemainders") {
    assertEquals(
      obtained = path.decodeWithRemainders(Chain("foobar", "foo", "42", "bar", "3")),
      expected = (Chain.empty, ("foobar", 42, 3L)).valid
    )
    assertEquals(
      obtained = path.decodeWithRemainders(Chain.empty),
      expected = Violations.oneNec(History.Root / "x", Constraint.required.toViolation(OpenApi.Null)).invalid
    )
    assertEquals(
      obtained = path.decodeWithRemainders(Chain("foobar", "foo", "42", "bar", "3", "?")),
      expected = (Chain("?"), ("foobar", 42, 3L)).valid
    )
    assertEquals(
      obtained = path.decodeWithRemainders(Chain("foobar", "foo", "NaN", "bar", "3")),
      expected = Violations
        .oneNec(History.Root / "y", Constraint.tpe("OpenApi.Int".asOpenApi).toViolation("NaN".asOpenApi))
        .invalid
    )
    assertEquals(
      obtained = path.decodeWithRemainders(Chain("?", "foobar", "foo", "42", "bar", "3")),
      expected = Violations.oneNec(History.Root / "foo", Constraint.required.toViolation("foobar".asOpenApi)).invalid
    )
  }

  test("decode: Path.Root") {
    assertEquals(
      obtained = Path.Root.decode(Chain.empty),
      expected = ().valid
    )
    assertEquals(
      obtained = Path.Root.decode(Chain("foobar")),
      expected = Violations.rootNec(Constraint.text.equal("/".asOpenApi).toViolation("/foobar".asOpenApi)).invalid
    )
  }

  test("encode") {
    assertEquals(
      obtained = path.encode(("foobar", 42, 3L)),
      expected = Chain("foobar", "foo", "42", "bar", "3")
    )
  }

  test("encode: Path.Root") {
    assertEquals(
      obtained = Path.Root.encode(()),
      expected = Chain.empty
    )
  }
