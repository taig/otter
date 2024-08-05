package io.taig.otter.http

import munit.FunSuite
import io.taig.otter.http.Dsl.*
import cats.syntax.all.*

final class RequestTest extends FunSuite:
  test("encode") {
    assertEquals(
      obtained = request(method.get, __ / "foo").encode(()),
      expected = Http.Request(
        method.get,
        Http.Url(path = Vector("foo"), queries = Http.Queries.Empty),
        headers = Http.Headers.Empty,
        body = Http.Payload.Empty
      )
    )

    assertEquals(
      obtained = request(method.get, __ / "foo" / segment("bar", int) & query("baz", string)).encode((42, "foobar")),
      expected = Http.Request(
        method.get,
        Http.Url(path = Vector("foo", "42"), queries = Vector("baz" -> "foobar".some)),
        headers = Http.Headers.Empty,
        body = Http.Payload.Empty
      )
    )
  }
