// package io.taig.otter.http

// import cats.syntax.all.*
// import io.taig.otter.http.Dsl.*
// import munit.FunSuite
// import org.typelevel.ci.*

// final class HeadersTest extends FunSuite:
//   test("encode"):
//     val codec = header(ci"foo", string) :* header(ci"bar", int)

//     assertEquals(
//       obtained = codec.encode(("foobar", 42)),
//       expected = Vector(ci"foo" -> "foobar", ci"bar" -> "42")
//     )

//   test("encode (optional)"):
//     val codec = header(ci"foo", string.nullable) :* header(ci"bar", int.nullable)

//     assertEquals(
//       obtained = codec.encode(("foobar".some, 42.some)),
//       expected = Vector(ci"foo" -> "foobar", ci"bar" -> "42")
//     )

//     assertEquals(
//       obtained = codec.encode((none, 42.some)),
//       expected = Vector(ci"bar" -> "42")
//     )

//     assertEquals(
//       obtained = codec.encode(("foobar".some, none)),
//       expected = Vector(ci"foo" -> "foobar")
//     )

//     assertEquals(
//       obtained = codec.encode((none, none)),
//       expected = Vector.empty
//     )

//   test("encode: array"):
//     val codec = header(ci"foo", collection.vector(string)).toHeaders

//     assertEquals(
//       obtained = codec.encode(Vector("foo", "bar", "baz")),
//       expected = Vector(ci"foo" -> "foo,bar,baz")
//     )

//   test("encode: object"):
//     val codec = header(ci"foo", field("foo", string) :* field("bar", int).optional).toHeaders

//     assertEquals(
//       obtained = codec.encode(("foobar", 42.some)),
//       expected = Vector(ci"foo" -> "foo=foobar&bar=42")
//     )

//     assertEquals(
//       obtained = codec.encode(("foobar", none)),
//       expected = Vector(ci"foo" -> "foo=foobar")
//     )
