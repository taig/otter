package io.taig.openapi.http

import cats.data.Chain
import cats.syntax.all.*
import io.taig.openapi.{History, OpenApi}
import io.taig.openapi.syntax.*
import io.taig.openapi.http.syntax.*
import io.taig.openapi.schema.schemas.*
import io.taig.openapi.schema.{Violations, Void}
import io.taig.openapi.validation.Constraint
import munit.FunSuite

import scala.collection.immutable.VectorMap

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
      obtained = queries.matches(VectorMap("x" -> "foo", "y" -> "42", "z" -> "3")),
      expected = true
    )
    assertEquals(
      obtained = queries.matches(VectorMap("x" -> "foo", "z" -> "3")),
      expected = true
    )
    assertEquals(
      obtained = queries.matches(VectorMap("x" -> "foo", "y" -> "42")),
      expected = false
    )
    assertEquals(
      obtained = queries.matches(VectorMap("x" -> "foo", "z" -> "3", "foo" -> "bar")),
      expected = true
    )
  }

  test("matches: Queries.Empty") {
    assertEquals(
      obtained = Queries.Empty.matches(VectorMap.empty),
      expected = true
    )
    assertEquals(
      obtained = Queries.Empty.matches(VectorMap("x" -> "foo")),
      expected = true
    )
  }

  test("matchesWithRemainders") {
    assertEquals(
      obtained = queries.matchesWithRemainders(VectorMap("x" -> "foo", "y" -> "42", "z" -> "3")),
      expected = (VectorMap.empty, true)
    )
    assertEquals(
      obtained = queries.matchesWithRemainders(VectorMap("x" -> "foo", "z" -> "3")),
      expected = (VectorMap.empty, true)
    )
    assertEquals(
      obtained = queries.matchesWithRemainders(VectorMap("x" -> "foo", "y" -> "42")),
      expected = (VectorMap.empty, false)
    )
    assertEquals(
      obtained = queries.matchesWithRemainders(VectorMap("x" -> "foo", "z" -> "3", "foo" -> "bar")),
      expected = (VectorMap("foo" -> "bar"), true)
    )
  }

  test("matchesWithRemainders: Queries.Empty") {
    assertEquals(
      obtained = Queries.Empty.matchesWithRemainders(VectorMap.empty),
      expected = (VectorMap.empty, true)
    )
    assertEquals(
      obtained = Queries.Empty.matchesWithRemainders(VectorMap("x" -> "foo")),
      expected = (VectorMap("x" -> "foo"), true)
    )
  }

  test("decode") {
    assertEquals(
      obtained = queries.decode(VectorMap("x" -> "foobar", "y" -> "42", "z" -> "3")),
      expected = ("foobar", 42.some, 3L).valid
    )
    assertEquals(
      obtained = queries.decode(VectorMap("x" -> "foobar", "z" -> "3")),
      expected = ("foobar", none, 3L).valid
    )
    assertEquals(
      obtained = queries.decode(VectorMap("x" -> "foobar", "z" -> "3", "?" -> "asdf")),
      expected = ("foobar", none, 3L).valid
    )
    assertEquals(
      obtained = queries.decode(VectorMap("x" -> "foobar")),
      expected = Violations
        .oneNec(
          History.Root / "z",
          Constraint.required.toViolation(OpenApi.Null)
        )
        .invalid
    )
    assertEquals(
      obtained = queries.decode(VectorMap("x" -> "foobar", "y" -> "asdf")),
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
      obtained = Queries.Empty.decode(VectorMap.empty),
      expected = Void.valid
    )
    assertEquals(
      obtained = Queries.Empty.decode(VectorMap("x" -> "foobar")),
      expected = Void.valid
    )
  }

  test("decodeWithRemainders: Queries.Empty") {
    assertEquals(
      obtained = Queries.Empty.decodeWithRemainders(VectorMap.empty),
      expected = (VectorMap.empty, Void).valid
    )
    assertEquals(
      obtained = Queries.Empty.decodeWithRemainders(VectorMap("x" -> "foobar")),
      expected = (VectorMap("x" -> "foobar"), Void).valid
    )
  }

  test("decodeWithRemainders") {
    assertEquals(
      obtained = queries.decodeWithRemainders(VectorMap("x" -> "foobar", "y" -> "42", "z" -> "3")),
      expected = (VectorMap.empty, ("foobar", 42.some, 3L)).valid
    )
    assertEquals(
      obtained = queries.decodeWithRemainders(VectorMap("x" -> "foobar", "y" -> "42", "z" -> "3", "?" -> "asdf")),
      expected = (VectorMap("?" -> "asdf"), ("foobar", 42.some, 3L)).valid
    )
  }

  test("encode") {
    assertEquals(
      obtained = queries.encode(("foobar", 42.some, 3L)),
      expected = VectorMap("x" -> "foobar", "y" -> "42", "z" -> "3")
    )
    assertEquals(
      obtained = queries.encode(("foobar", none, 3L)),
      expected = VectorMap("x" -> "foobar", "z" -> "3")
    )
  }

  test("encode: Queries.Empty") {
    assertEquals(
      obtained = Queries.Empty.encode(Void),
      expected = VectorMap.empty
    )
  }
