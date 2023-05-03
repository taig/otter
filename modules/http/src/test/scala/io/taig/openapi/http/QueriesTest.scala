package io.taig.openapi.http

import io.taig.openapi.http.syntax.*
import io.taig.openapi.schema.schemas.*
import munit.FunSuite

import scala.collection.immutable.VectorMap

final class QueriesTest extends FunSuite:
  val queries: Queries[(String, Option[Int], Long)] = query("x", string) & query("y", int).optional & query("z", long)

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
