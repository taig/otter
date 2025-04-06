// package io.taig.otter.http

// import cats.syntax.all.*
// import io.taig.otter.XPath
// import io.taig.otter.http.Dsl.*
// import munit.FunSuite
// import org.typelevel.ci.*

// import java.nio.charset.StandardCharsets

// final class RequestTest extends FunSuite:
//   test("encode"):
//     assertEquals(
//       obtained = request(method.get, __ / "foo").encode(contentType = none, ()),
//       expected = Http.Request(
//         method = method.get,
//         url = Http.Url(path = Vector("foo"), queries = Http.Queries.Empty),
//         headers = Http.Headers.Empty,
//         body = Array.emptyByteArray
//       )
//     )

//     assertEquals(
//       obtained = request(method.get, __ / "foo" / parameter("bar", int) & query("baz", string))
//         .encode(contentType = none, (42, "foobar")),
//       expected = Http.Request(
//         method = method.get,
//         url = Http.Url(path = Vector("foo", "42"), queries = Vector("baz" -> "foobar".some)),
//         headers = Http.Headers.Empty,
//         body = Array.emptyByteArray
//       )
//     )

//   test("encode: body (binary)"):
//     val obtained = request(method.get, __, binary).encode(contentType = none, Array(1, 2, 3).map(_.toByte))

//     assertEquals(
//       obtained = obtained.headers,
//       expected = Vector(ci"Content-Type" -> "application/octet-stream")
//     )

//     assertEquals(
//       obtained = obtained.body.toVector,
//       expected = Vector(1, 2, 3).map(_.toByte)
//     )

//   test("encode: body (text)"):
//     val obtained = request(method.get, __, text(string)).encode(contentType = none, "foobar")

//     assertEquals(
//       obtained = obtained.headers,
//       expected = Vector(ci"Content-Type" -> "text/plain")
//     )

//     assertEquals(
//       obtained = obtained.body.toVector,
//       expected = "foobar".getBytes(StandardCharsets.UTF_8).toVector
//     )

//   test("encode: body (formData)"):
//     val codec = field("foo", string) :* field("bar", int)
//     val obtained = request(method.get, __, formData(codec)).encode(contentType = none, ("foobar", 42))

//     assertEquals(
//       obtained = obtained.headers,
//       expected = Vector(ci"Content-Type" -> "application/x-www-form-urlencoded")
//     )

//     assertEquals(
//       obtained = obtained.body.toVector,
//       expected = "foo=foobar&bar=42".getBytes(StandardCharsets.UTF_8).toVector
//     )

//   test("encode: body (text orElse formData)"):
//     val bodies = formData(field("foo", string) :* field("bar", int)) :+ text(string)
//     val codec = request(method.get, __, bodies)

//     val obtainedFormData = codec.encode(contentType = none, Left(("foobar", 42)))
//     val obtainedText = codec.encode(contentType = none, Right("foobar"))

//     assertEquals(
//       obtained = obtainedFormData.headers,
//       expected = Vector(ci"Content-Type" -> "application/x-www-form-urlencoded")
//     )

//     assertEquals(
//       obtained = obtainedFormData.body.toVector,
//       expected = "foo=foobar&bar=42".getBytes(StandardCharsets.UTF_8).toVector
//     )

//     assertEquals(
//       obtained = obtainedText.headers,
//       expected = Vector(ci"Content-Type" -> "text/plain")
//     )

//     assertEquals(
//       obtained = obtainedText.body.toVector,
//       expected = "foobar".getBytes(StandardCharsets.UTF_8).toVector
//     )

//   test("encode: body (text or formData)"):
//     val bodies = formData(field("foo", string).toRecord) + text(string)
//     val codec = request(method.get, __, bodies)

//     val obtainedFormData = codec.encode(contentType = mediaType.application.wwwFormUrlencoded.some, "foobar")
//     val obtainedText = codec.encode(contentType = mediaType.text.plain.some, "foobar")

//     assertEquals(
//       obtained = obtainedFormData.headers,
//       expected = Vector(ci"Content-Type" -> "application/x-www-form-urlencoded")
//     )

//     assertEquals(
//       obtained = obtainedFormData.body.toVector,
//       expected = "foo=foobar".getBytes(StandardCharsets.UTF_8).toVector
//     )

//     assertEquals(
//       obtained = obtainedText.headers,
//       expected = Vector(ci"Content-Type" -> "text/plain")
//     )

//     assertEquals(
//       obtained = obtainedText.body.toVector,
//       expected = "foobar".getBytes(StandardCharsets.UTF_8).toVector
//     )

//   test("decode"):
//     assertEquals(
//       obtained = request(method.get, __ / "foo").decode(
//         Http.Request(
//           method = method.get,
//           url = Http.Url(path = Vector("foo"), queries = Http.Queries.Empty),
//           headers = Http.Headers.Empty,
//           body = Array.emptyByteArray
//         )
//       ),
//       expected = ().asRight
//     )

//     assertEquals(
//       obtained = request(method.get, __ / "foo" / parameter("bar", int) & query("baz", string))
//         .decode(
//           Http.Request(
//             method = method.get,
//             url = Http.Url(path = Vector("foo", "42"), queries = Vector("baz" -> "foobar".some)),
//             headers = Http.Headers.Empty,
//             body = Array.emptyByteArray
//           )
//         ),
//       expected = (42, "foobar").asRight
//     )

//   test("decode: body (text orElse formData)"):
//     val bodies = formData(field("foo", string) :* field("bar", int)) :+ text(string)
//     val codec = request(method.get, __, bodies)

//     assertEquals(
//       obtained = codec.decode(
//         Http.Request(
//           method = method.get,
//           url = Http.Url.Empty,
//           headers = Vector(ci"Content-Type" -> "application/x-www-form-urlencoded"),
//           body = "foo=foobar&bar=42".getBytes(StandardCharsets.UTF_8)
//         )
//       ),
//       expected = ("foobar", 42).asLeft.asRight
//     )

//     assertEquals(
//       obtained = codec.decode(
//         Http.Request(
//           method = method.get,
//           url = Http.Url.Empty,
//           headers = Vector(ci"Content-Type" -> "text/plain"),
//           body = "foobar".getBytes(StandardCharsets.UTF_8)
//         )
//       ),
//       expected = "foobar".asRight.asRight
//     )

//     assertEquals(
//       obtained = codec.decode(
//         Http.Request(
//           method = method.get,
//           url = Http.Url.Empty,
//           headers = Http.Headers.Empty,
//           body = "foobar".getBytes(StandardCharsets.UTF_8)
//         )
//       ),
//       expected = Route.Error
//         .MediaTypesUnsupported(
//           Violations.namespaceNec(XPath.Root / "header" / "Content-Type", Violation.tpe("string", actual = "null"))
//         )
//         .asLeft
//     )

//     assertEquals(
//       obtained = codec.decode(
//         Http.Request(
//           method = method.get,
//           url = Http.Url.Empty,
//           headers = Vector(ci"Content-Type" -> "application/json"),
//           body = "foobar".getBytes(StandardCharsets.UTF_8)
//         )
//       ),
//       expected = Route.Error
//         .MediaTypesUnsupported(
//           Violations.rootNec(
//             Violation.oneOf(List("application/x-www-form-urlencoded", "text/plain"), "application/json")
//           )
//         )
//         .asLeft
//     )

//     assertEquals(
//       obtained = codec.decode(
//         Http.Request(
//           method = method.get,
//           url = Http.Url.Empty,
//           headers = Vector(ci"Content-Type" -> "foobar"),
//           body = "foobar".getBytes(StandardCharsets.UTF_8)
//         )
//       ),
//       expected = Route.Error
//         .MediaTypesUnsupported(
//           Violations.namespaceNec(XPath.Root / "header" / "Content-Type", Violation.tpe("mediaType", actual = "foobar"))
//         )
//         .asLeft
//     )
