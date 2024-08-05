package io.taig.otter.http

import munit.FunSuite
import io.taig.otter.http.Dsl.*
import cats.syntax.all.*

final class QueriesTest extends FunSuite:
  test("encode"):
    val queries = query("foo", int) :* query("bar", string).optional

    assertEquals(
      obtained = queries.encode((42, "foobar".some)),
      expected = Vector("foo" -> "42".some, "bar" -> "foobar".some)
    )

    assertEquals(
      obtained = queries.encode((42, none)),
      expected = Vector("foo" -> "42".some, "bar" -> none)
    )

  test("encode: array"):
    val queries = query("foo", collection.vector(string)).toQueries

    assertEquals(
      obtained = queries.encode(Vector("bar", "baz")),
      expected = Vector("foo" -> "bar,baz".some)
    )

    assertEquals(
      obtained = queries.encode(Vector()),
      expected = Vector("foo" -> none)
    )

  test("encode: object"):
    val queries = query("foo", record(field("bar", string.optional))).toQueries

    assertEquals(
      obtained = queries.encode("baz".some),
      expected = Vector("foo" -> "bar=baz".some)
    )

    assertEquals(
      obtained = queries.encode(none),
      expected = Vector("foo" -> none)
    )
