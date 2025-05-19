// package io.taig.otter.http

// import io.taig.otter.OtterSuite
// import io.taig.otter.http.HttpHeaderDsl.*

// final class HttpHeaderPrinterTest extends OtterSuite:
//   test("collection"):
//     assertEq(
//       obtained = HttpHeaderPrinter(explode = false)(collection.list(string), List("foo", "bar")),
//       expected = "foo,bar"
//     )

//   // TODO this is problematic; should we only allow non-empty collections (?)
//   test("collection: empty"):
//     assertEq(
//       obtained = HttpHeaderPrinter(explode = false)(collection.list(string), Nil),
//       expected = ""
//     )

//   test("collection: escape"):
//     assertEq(
//       obtained = HttpHeaderPrinter(explode = false)(collection.list(string), List("foo", "foo,bar", "bar")),
//       expected = "foo,foo\\,bar,bar"
//     )

//   test("constant"):
//     assertEq(
//       obtained = HttpHeaderPrinter(explode = false)(constant(string, "foo"), "bar"),
//       expected = "foo"
//     )

//   test("object: explode = false"):
//     val codec = field("foo", string) :* field("bar", string)

//     assertEq(
//       obtained = HttpHeaderPrinter(explode = false)(codec, ("x", "y")),
//       expected = "foo,x,bar,y"
//     )

//   test("object: explode = false (escape)"):
//     val codec = field("foo", string) :* field("bar", string)

//     assertEq(
//       obtained = HttpHeaderPrinter(explode = false)(codec, ("a,b,c", "x=y")),
//       expected = "foo,a\\,b\\,c,bar,x=y"
//     )

//   test("object: explode = true"):
//     val codec = field("foo", string) :* field("bar", string)

//     assertEq(
//       obtained = HttpHeaderPrinter(explode = true)(codec, ("x", "y")),
//       expected = "foo=x,bar=y"
//     )

//   test("object: explode = true (escape)"):
//     val codec = field("foo", string) :* field("bar", string)

//     assertEq(
//       obtained = HttpHeaderPrinter(explode = true)(codec, ("a,b,c", "x=y")),
//       expected = "foo=a\\,b\\,c,bar=x\\=y"
//     )

//   test("primitive"):
//     assertEq(
//       obtained = HttpHeaderPrinter(explode = false)(string, "foobar"),
//       expected = "foobar"
//     )

//   test("tuple"):
//     assertEq(
//       obtained = HttpHeaderPrinter(explode = false)(string :* string, ("foo", "bar")),
//       expected = "foo,bar"
//     )

//   test("tuple: escape"):
//     assertEq(
//       obtained = HttpHeaderPrinter(explode = false)(string :* string :* string, ("foo", "foo,bar", "bar")),
//       expected = "foo,foo\\,bar,bar"
//     )
