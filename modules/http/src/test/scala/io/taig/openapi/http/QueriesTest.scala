package io.taig.openapi.http

import cats.data.Chain
import cats.syntax.all.*
import io.taig.openapi.{History, OpenApi}
import io.taig.openapi.syntax.*
import io.taig.openapi.http.syntax.*
import io.taig.openapi.schema.schemas.*
import io.taig.openapi.schema.Violations
import io.taig.openapi.validation.Constraint
import munit.FunSuite

final class QueriesTest extends FunSuite:
  val x: Query[String] = query("x", string)
  val y: Query[Option[Int]] = query("y", int).optional
  val z: Query[Long] = query("z", long)
  val queries: Queries[(String, Option[Int], Long)] = x & y & z

  test("toChain") {
    assertEquals(obtained = queries.toChain, expected = Chain(x, y, z))
    assertEquals(obtained = Queries.Empty.toChain, expected = Chain.empty)
  }

  test("matches") {
    assertEquals(
      obtained = queries.matches(Http.Queries.of("x" -> "foo", "y" -> "42", "z" -> "3")),
      expected = true
    )
    assertEquals(
      obtained = queries.matches(Http.Queries.of("x" -> "foo", "z" -> "3")),
      expected = true
    )
    assertEquals(
      obtained = queries.matches(Http.Queries.of("x" -> "foo", "y" -> "42")),
      expected = false
    )
    assertEquals(
      obtained = queries.matches(Http.Queries.of("x" -> "foo", "z" -> "3", "foo" -> "bar")),
      expected = true
    )
  }

  test("matches: Queries.Empty") {
    assertEquals(
      obtained = Queries.Empty.matches(Http.Queries.Empty),
      expected = true
    )
    assertEquals(
      obtained = Queries.Empty.matches(Http.Queries.of("x" -> "foo")),
      expected = true
    )
  }

  test("matchesWithRemainders") {
    assertEquals(
      obtained = queries.matchesWithRemainders(Http.Queries.of("x" -> "foo", "y" -> "42", "z" -> "3")),
      expected = (Http.Queries.Empty, true)
    )
    assertEquals(
      obtained = queries.matchesWithRemainders(Http.Queries.of("x" -> "foo", "z" -> "3")),
      expected = (Http.Queries.Empty, true)
    )
    assertEquals(
      obtained = queries.matchesWithRemainders(Http.Queries.of("x" -> "foo", "y" -> "42")),
      expected = (Http.Queries.Empty, false)
    )
    assertEquals(
      obtained = queries.matchesWithRemainders(Http.Queries.of("x" -> "foo", "z" -> "3", "foo" -> "bar")),
      expected = (Http.Queries.of("foo" -> "bar"), true)
    )
  }

  test("matchesWithRemainders: Queries.Empty") {
    assertEquals(
      obtained = Queries.Empty.matchesWithRemainders(Http.Queries.Empty),
      expected = (Http.Queries.Empty, true)
    )
    assertEquals(
      obtained = Queries.Empty.matchesWithRemainders(Http.Queries.of("x" -> "foo")),
      expected = (Http.Queries.of("x" -> "foo"), true)
    )
  }

  test("decode") {
    assertEquals(
      obtained = queries.decode(Http.Queries.of("x" -> "foobar", "y" -> "42", "z" -> "3")),
      expected = ("foobar", 42.some, 3L).valid
    )
    assertEquals(
      obtained = queries.decode(Http.Queries.of("x" -> "foobar", "z" -> "3")),
      expected = ("foobar", none, 3L).valid
    )
    assertEquals(
      obtained = queries.decode(Http.Queries.of("x" -> "foobar", "z" -> "3", "?" -> "asdf")),
      expected = ("foobar", none, 3L).valid
    )
    assertEquals(
      obtained = queries.decode(Http.Queries.of("x" -> "foobar")),
      expected = Violations
        .oneNec(
          History.Root / "z",
          Constraint.required.toViolation(OpenApi.Null)
        )
        .invalid
    )
    assertEquals(
      obtained = queries.decode(Http.Queries.of("x" -> "foobar", "y" -> "asdf")),
      expected = Violations
        .ofNec(
          History.Root / "y" -> Constraint.tpe("OpenApi.Int".asOpenApi).toViolation("asdf".asOpenApi),
          History.Root / "z" -> Constraint.required.toViolation(OpenApi.Null)
        )
        .invalid
    )
  }

  test("decode: Queries.Empty") {
    assertEquals(
      obtained = Queries.Empty.decode(Http.Queries.Empty),
      expected = ().valid
    )
    assertEquals(
      obtained = Queries.Empty.decode(Http.Queries.of("x" -> "foobar")),
      expected = ().valid
    )
  }

  test("decodeWithRemainders: Queries.Empty") {
    assertEquals(
      obtained = Queries.Empty.decodeWithRemainders(Http.Queries.Empty),
      expected = (Http.Queries.Empty, ()).valid
    )
    assertEquals(
      obtained = Queries.Empty.decodeWithRemainders(Http.Queries.of("x" -> "foobar")),
      expected = (Http.Queries.of("x" -> "foobar"), ()).valid
    )
  }

  test("decodeWithRemainders") {
    assertEquals(
      obtained = queries.decodeWithRemainders(Http.Queries.of("x" -> "foobar", "y" -> "42", "z" -> "3")),
      expected = (Http.Queries.Empty, ("foobar", 42.some, 3L)).valid
    )
    assertEquals(
      obtained = queries.decodeWithRemainders(Http.Queries.of("x" -> "foobar", "y" -> "42", "z" -> "3", "?" -> "asdf")),
      expected = (Http.Queries.of("?" -> "asdf"), ("foobar", 42.some, 3L)).valid
    )
  }

  test("encode") {
    assertEquals(
      obtained = queries.encode(("foobar", 42.some, 3L)),
      expected = Http.Queries.of("x" -> "foobar", "y" -> "42", "z" -> "3")
    )
    assertEquals(
      obtained = queries.encode(("foobar", none, 3L)),
      expected = Http.Queries.of("x" -> "foobar", "z" -> "3")
    )
  }

  test("encode: Queries.Empty") {
    assertEquals(
      obtained = Queries.Empty.encode(()),
      expected = Http.Queries.Empty
    )
  }
