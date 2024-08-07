package io.taig.otter.http

import munit.FunSuite
import io.taig.otter.http.Dsl.*
import cats.syntax.all.*
import org.typelevel.ci.*
import java.nio.charset.StandardCharsets
import cats.data.NonEmptyList

final class RequestTest extends FunSuite:
  test("encode"):
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

  test("encode: body (binary)"):
    val obtained = request(method.get, __, binary).encode(charset = none, Array(1, 2, 3).map(_.toByte))

    assertEquals(
      obtained = obtained.headers,
      expected = Vector(ci"Content-Type" -> "application/octet-stream")
    )

    assertEquals(
      obtained = obtained.body.data.toVector,
      expected = Vector(1, 2, 3).map(_.toByte)
    )

  test("encode: body (text)"):
    val obtained = request(method.get, __, text(string)).encode(charset = none, "foobar")

    assertEquals(
      obtained = obtained.headers,
      expected = Vector(ci"Content-Type" -> "text/plain")
    )

    assertEquals(
      obtained = obtained.body.data.toVector,
      expected = "foobar".getBytes(StandardCharsets.UTF_8).toVector
    )

  test("encode: body (formData)"):
    val codec = record(field("foo", string) :* field("bar", int))
    val obtained = request(method.get, __, formData(codec)).encode(charset = none, ("foobar", 42))

    assertEquals(
      obtained = obtained.headers,
      expected = Vector(ci"Content-Type" -> "application/x-www-form-urlencoded")
    )

    assertEquals(
      obtained = obtained.body.data.toVector,
      expected = "foo=foobar&bar=42".getBytes(StandardCharsets.UTF_8).toVector
    )

  test("encode: body (text & formData)"):
    val bodies = formData(record(field("foo", string) :* field("bar", int))) :+ text(string)
    val codec = request(method.get, __, bodies)

    val obtainedFormData = codec.encode(charset = none, Left(("foobar", 42)))
    val obtainedText = codec.encode(charset = none, Right("foobar"))

    assertEquals(
      obtained = obtainedFormData.headers,
      expected = Vector(ci"Content-Type" -> "application/x-www-form-urlencoded")
    )

    assertEquals(
      obtained = obtainedFormData.body.data.toVector,
      expected = "foo=foobar&bar=42".getBytes(StandardCharsets.UTF_8).toVector
    )

    assertEquals(
      obtained = obtainedText.headers,
      expected = Vector(ci"Content-Type" -> "text/plain")
    )

    assertEquals(
      obtained = obtainedText.body.data.toVector,
      expected = "foobar".getBytes(StandardCharsets.UTF_8).toVector
    )

  test("decode"):
    assertEquals(
      obtained = request(method.get, __ / "foo").decode(
        Http.Request(
          method = method.get,
          url = Http.Url(path = Vector("foo"), queries = Http.Queries.Empty),
          headers = Http.Headers.Empty,
          body = Http.Payload.Empty
        )
      ),
      expected = Request.Result.Success(())
    )

    assertEquals(
      obtained = request(method.get, __ / "foo" / segment("bar", int) & query("baz", string))
        .decode(
          Http.Request(
            method = method.get,
            url = Http.Url(path = Vector("foo", "42"), queries = Vector("baz" -> "foobar".some)),
            headers = Http.Headers.Empty,
            body = Http.Payload.Empty
          )
        ),
      expected = Request.Result.Success((42, "foobar"))
    )

  test("decode: body (text & formData)"):
    val bodies = formData(record(field("foo", string) :* field("bar", int))) :+ text(string)
    val codec = request(method.get, __, bodies)

    assertEquals(
      obtained = codec.decode(
        Http.Request(
          method = method.get,
          url = Http.Url.Empty,
          headers = Vector(ci"Content-Type" -> "application/x-www-form-urlencoded"),
          body = Http.Payload("foo=foobar&bar=42".getBytes(StandardCharsets.UTF_8))
        )
      ),
      expected = Request.Result.Success(("foobar", 42).asLeft)
    )

    assertEquals(
      obtained = codec.decode(
        Http.Request(
          method = method.get,
          url = Http.Url.Empty,
          headers = Vector(ci"Content-Type" -> "text/plain"),
          body = Http.Payload("foobar".getBytes(StandardCharsets.UTF_8))
        )
      ),
      expected = Request.Result.Success("foobar".asRight)
    )

    assertEquals(
      obtained = codec.decode(
        Http.Request(
          method = method.get,
          url = Http.Url.Empty,
          headers = Http.Headers.Empty,
          body = Http.Payload("foobar".getBytes(StandardCharsets.UTF_8))
        )
      ),
      expected = Request.Result.MediaTypesUnsupported(
        Violations.rootNec(Violation(Constraint.Type("string"), actual = Data.String("null")))
      )
    )

    assertEquals(
      obtained = codec.decode(
        Http.Request(
          method = method.get,
          url = Http.Url.Empty,
          headers = Vector(ci"Content-Type" -> "application/json"),
          body = Http.Payload("foobar".getBytes(StandardCharsets.UTF_8))
        )
      ),
      expected = Request.Result.MediaTypesUnsupported(
        Violations.rootNec(
          Violation(
            Constraint.OneOf(NonEmptyList.of("application/x-www-form-urlencoded", "text/plain").map(Data.String.apply)),
            actual = Data.String("application/json")
          )
        )
      )
    )

    assertEquals(
      obtained = codec.decode(
        Http.Request(
          method = method.get,
          url = Http.Url.Empty,
          headers = Vector(ci"Content-Type" -> "foobar"),
          body = Http.Payload("foobar".getBytes(StandardCharsets.UTF_8))
        )
      ),
      expected = Request.Result.MediaTypesUnsupported(
        Violations.rootNec(Violation(Constraint.Type("mediaType"), actual = Data.String("foobar")))
      )
    )
