package io.taig.openapi.http

import cats.data.Chain
import io.taig.openapi.http.syntax.*
import io.taig.openapi.schema.schemas.*
import munit.FunSuite

final class PathTest extends FunSuite:
  val path: Path[(String, Int, Long)] =
    Path.Root / parameter("x", string) / "foo" / parameter("y", int) / "bar" / parameter("z", long)

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