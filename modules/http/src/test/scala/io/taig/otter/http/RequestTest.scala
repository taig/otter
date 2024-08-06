package io.taig.otter.http

import munit.FunSuite
import io.taig.otter.http.Dsl.*
import cats.syntax.all.*
import org.typelevel.ci.*
import java.nio.charset.StandardCharsets

final class RequestTest extends FunSuite:
  test("encode") {
    assertEquals(
      obtained = request(method.get, __ / "foo").encode(charset = none, ()),
      expected = Http.Request(
        method = method.get,
        url = Http.Url(path = Vector("foo"), queries = Http.Queries.Empty),
        headers = Http.Headers.Empty,
        body = Http.Payload.Empty
      )
    )

    assertEquals(
      obtained = request(method.get, __ / "foo" / segment("bar", int) & query("baz", string))
        .encode(charset = none, (42, "foobar")),
      expected = Http.Request(
        method = method.get,
        url = Http.Url(path = Vector("foo", "42"), queries = Vector("baz" -> "foobar".some)),
        headers = Http.Headers.Empty,
        body = Http.Payload.Empty
      )
    )
  }

  test("encode: body (binary)") {
    val obtained = request(method.get, __, binary.input).encode(charset = none, Array(1, 2, 3).map(_.toByte))

    assertEquals(
      obtained = obtained.headers,
      expected = Vector(ci"Content-Type" -> "application/octet-stream")
    )

    assertEquals(
      obtained = obtained.body.data.toVector,
      expected = Vector(1, 2, 3).map(_.toByte)
    )
  }

  test("encode: body (formData)") {
    val codec = record(field("foo", string) :* field("bar", int))
    val obtained = request(method.get, __, formData.input(codec)).encode(charset = none, ("foobar", 42))

    assertEquals(
      obtained = obtained.headers,
      expected = Vector(ci"Content-Type" -> "application/x-www-form-urlencoded")
    )

    assertEquals(
      obtained = obtained.body.data.toVector,
      expected = "foo=foobar&bar=42".getBytes(StandardCharsets.UTF_8).toVector
    )
  }
