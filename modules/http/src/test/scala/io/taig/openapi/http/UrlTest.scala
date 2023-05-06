package io.taig.openapi.http

import cats.data.Chain
import cats.syntax.all.*
import io.taig.openapi.http.syntax.*
import io.taig.openapi.schema.Void
import io.taig.openapi.schema.schemas.*
import munit.FunSuite

import scala.collection.immutable.VectorMap

final class UrlTest extends FunSuite:
  val url: Url[(String, Int, String, Option[Int], Long)] =
    __ / "foo" / parameter("a", string) / "bar" / parameter("b", int)
      & query("x", string)
      & query("y", int).optional
      & query("z", long)

  test("matches") {
    assertEquals(
      obtained = url.matches(path = Chain.empty, queries = VectorMap.empty),
      expected = false
    )
    assertEquals(
      obtained = url.matches(
        path = Chain("foo", "xxx", "bar", "42"),
        queries = VectorMap("x" -> "42", "y" -> "NaN", "z" -> "foobar")
      ),
      expected = true
    )
    assertEquals(
      obtained = url.matches(
        path = Chain("foo", "xxx", "bar", "42"),
        queries = VectorMap("x" -> "42", "z" -> "foobar")
      ),
      expected = true
    )
    assertEquals(
      obtained = url.matches(
        path = Chain("oof", "xxx", "bar", "42"),
        queries = VectorMap("x" -> "42", "z" -> "foobar")
      ),
      expected = false
    )
    assertEquals(
      obtained = url.matches(
        path = Chain("foo", "xxx", "rab", "42"),
        queries = VectorMap("x" -> "42", "z" -> "foobar")
      ),
      expected = false
    )
    assertEquals(
      obtained = url.matches(
        path = Chain("foo", "bar"),
        queries = VectorMap("x" -> "42", "z" -> "foobar")
      ),
      expected = false
    )
    assertEquals(
      obtained = url.matches(
        path = Chain("foo", "xxx", "bar", "42"),
        queries = VectorMap("x" -> "42")
      ),
      expected = false
    )
  }

  test("matches: Url.Root") {
    assertEquals(
      obtained = Url.Root.matches(path = Chain.empty, queries = VectorMap.empty),
      expected = true
    )
    assertEquals(
      obtained = Url.Root.matches(path = Chain.empty, queries = VectorMap("foo" -> "bar")),
      expected = true
    )
    assertEquals(
      obtained = Url.Root.matches(path = Chain("foobar"), queries = VectorMap.empty),
      expected = false
    )
  }

  test("decodeWithRemainders") {
    assertEquals(
      obtained = url.decodeWithRemainders(
        path = Chain("foo", "xxx", "bar", "42"),
        queries = VectorMap("x" -> "foobar", "y" -> "42", "z" -> "3")
      ),
      expected = (Chain.empty, VectorMap.empty, ("xxx", 42, "foobar", 42.some, 3L)).valid
    )
  }

  test("decodeWithRemainders: query validation error") {
    assertEquals(
      obtained = (Url.Root & query("foo", int))
        .decodeWithRemainders(path = Chain.empty, queries = VectorMap("foo" -> "42")),
      expected = (Chain.empty, VectorMap.empty, 42).valid
    )
  }

  test("decodeWithRemainders: Url.Empty") {
    assertEquals(
      obtained = Url.Root.decodeWithRemainders(path = Chain.empty, queries = VectorMap.empty),
      expected = (Chain.empty, VectorMap.empty, Void).valid
    )
    assertEquals(
      obtained = Url.Root.decodeWithRemainders(path = Chain.empty, queries = VectorMap("foo" -> "bar")),
      expected = (Chain.empty, VectorMap("foo" -> "bar"), Void).valid
    )
    assertEquals(
      obtained = Url.Root.decodeWithRemainders(path = Chain("foobar"), queries = VectorMap.empty),
      expected = (Chain("foobar"), VectorMap.empty, Void).valid
    )
  }

  test("encode") {
    assertEquals(
      obtained = url.encode(("foo", 42, "bar", 42.some, 3L)),
      expected = (Chain("foo", "foo", "bar", "42"), VectorMap("x" -> "bar", "y" -> "42", "z" -> "3"))
    )
    assertEquals(
      obtained = url.encode(("foo", 42, "bar", none, 3L)),
      expected = (Chain("foo", "foo", "bar", "42"), VectorMap("x" -> "bar", "z" -> "3"))
    )
  }

  test("encode: Url.Root") {
    assertEquals(
      obtained = Url.Root.encode(Void),
      expected = (Chain.empty, VectorMap.empty)
    )
  }
