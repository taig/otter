package io.taig.otter.http

import cats.syntax.all.*
import io.taig.otter.XPath
import io.taig.otter.http.Dsl.*
import munit.FunSuite

final class QueriesTest extends FunSuite:
  test("encode"):
    val queries = query("foo", int) :* query("bar", string).optional

    assertEquals(
      obtained = queries.encode((42, "foobar".some)),
      expected = Vector("foo" -> "42".some, "bar" -> "foobar".some)
    )

    assertEquals(
      obtained = queries.encode((42, none)),
      expected = Vector("foo" -> "42".some)
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
    val queries = query("foo", field("bar", string.nullable).toRecord).toQueries

    assertEquals(
      obtained = queries.encode("baz".some),
      expected = Vector("foo" -> "bar=baz".some)
    )

    assertEquals(
      obtained = queries.encode(none),
      expected = Vector("foo" -> "".some)
    )

  test("decode"):
    val queries = query("foo", int) :* query("bar", string).optional

    assertEquals(
      obtained = queries.decode(Vector("foo" -> "42".some, "bar" -> "foobar".some)),
      expected = (Vector.empty, (42, "foobar".some).valid)
    )

    assertEquals(
      obtained = queries.decode(Vector("foo" -> "42".some, "bar" -> none)),
      expected = (Vector.empty, (42, none).valid)
    )

    assertEquals(
      obtained = queries.decode(Vector("foo" -> "42".some)),
      expected = (Vector.empty, (42, none).valid)
    )

    assertEquals(
      obtained = queries.decode(Vector("foo" -> "foobar".some)),
      expected = (
        Vector.empty,
        Violations
          .namespaceNec(XPath.Root / "foo", Violation(Constraint.Type("int"), actual = String("string")))
          .invalid
      )
    )

  test("decode: array"):
    val queries = query("foo", collection.vector(string)).toQueries

    assertEquals(
      obtained = queries.decode(Vector("foo" -> "bar,baz".some)),
      expected = (Vector.empty, Vector("bar", "baz").valid)
    )

    assertEquals(
      obtained = queries.decode(Vector("foo" -> "".some)),
      expected = (Vector.empty, Vector("").valid)
    )

    assertEquals(
      obtained = queries.decode(Vector("foo" -> none)),
      expected = (Vector.empty, Vector().valid)
    )

    // not sure if this holds
    // assertEquals(
    //   obtained = queries.decode(Vector()),
    //   expected = (
    //     Vector.empty,
    //     Violations
    //       .namespaceNec(XPath.Root / "foo", Violation(Constraint.Type("array"), actual = String("null")))
    //       .invalid
    //   )
    // )
