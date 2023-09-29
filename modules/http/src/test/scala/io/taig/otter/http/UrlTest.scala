//package io.taig.otter.http
//
//import cats.data.Chain
//import cats.syntax.all.*
//import munit.FunSuite
//
//final class UrlTest extends FunSuite:
//  val url: Url[(String, Int, String, Option[Int], Long)] =
//    __ / "foo" / parameter("a", string) / "bar" / parameter("b", int)
//      & query("x", string)
//      & query("y", int).optional
//      & query("z", long)
//
//  test("matches") {
//    assertEquals(
//      obtained = url.matches(path = Chain.empty, queries = Http.Queries.Empty),
//      expected = false
//    )
//    assertEquals(
//      obtained = url.matches(
//        path = Chain("foo", "xxx", "bar", "42"),
//        queries = Http.Queries.of("x" -> "42", "y" -> "NaN", "z" -> "foobar")
//      ),
//      expected = true
//    )
//    assertEquals(
//      obtained = url.matches(
//        path = Chain("foo", "xxx", "bar", "42"),
//        queries = Http.Queries.of("x" -> "42", "z" -> "foobar")
//      ),
//      expected = true
//    )
//    assertEquals(
//      obtained = url.matches(
//        path = Chain("oof", "xxx", "bar", "42"),
//        queries = Http.Queries.of("x" -> "42", "z" -> "foobar")
//      ),
//      expected = false
//    )
//    assertEquals(
//      obtained = url.matches(
//        path = Chain("foo", "xxx", "rab", "42"),
//        queries = Http.Queries.of("x" -> "42", "z" -> "foobar")
//      ),
//      expected = false
//    )
//    assertEquals(
//      obtained = url.matches(
//        path = Chain("foo", "bar"),
//        queries = Http.Queries.of("x" -> "42", "z" -> "foobar")
//      ),
//      expected = false
//    )
//    assertEquals(
//      obtained = url.matches(
//        path = Chain("foo", "xxx", "bar", "42"),
//        queries = Http.Queries.of("x" -> "42")
//      ),
//      expected = false
//    )
//  }
//
//  test("matches: Url.Root") {
//    assertEquals(
//      obtained = Url.Root.matches(path = Chain.empty, queries = Http.Queries.Empty),
//      expected = true
//    )
//    assertEquals(
//      obtained = Url.Root.matches(path = Chain.empty, queries = Http.Queries.of("foo" -> "bar")),
//      expected = true
//    )
//    assertEquals(
//      obtained = Url.Root.matches(path = Chain("foobar"), queries = Http.Queries.Empty),
//      expected = false
//    )
//  }
//
//  test("decodeWithRemainders") {
//    assertEquals(
//      obtained = url.decodeWithRemainders(
//        path = Chain("foo", "xxx", "bar", "42"),
//        queries = Http.Queries.of("x" -> "foobar", "y" -> "42", "z" -> "3")
//      ),
//      expected = (Chain.empty, Http.Queries.Empty, ("xxx", 42, "foobar", 42.some, 3L)).valid
//    )
//  }
//
//  test("decodeWithRemainders: query validation error") {
//    assertEquals(
//      obtained = (Url.Root & query("foo", int))
//        .decodeWithRemainders(path = Chain.empty, queries = Http.Queries.of("foo" -> "42")),
//      expected = (Chain.empty, Http.Queries.Empty, 42).valid
//    )
//  }
//
//  test("decodeWithRemainders: Url.Empty") {
//    assertEquals(
//      obtained = Url.Root.decodeWithRemainders(path = Chain.empty, queries = Http.Queries.Empty),
//      expected = (Chain.empty, Http.Queries.Empty, ()).valid
//    )
//    assertEquals(
//      obtained = Url.Root.decodeWithRemainders(path = Chain.empty, queries = Http.Queries.of("foo" -> "bar")),
//      expected = (Chain.empty, Http.Queries.of("foo" -> "bar"), ()).valid
//    )
//    assertEquals(
//      obtained = Url.Root.decodeWithRemainders(path = Chain("foobar"), queries = Http.Queries.Empty),
//      expected = (Chain("foobar"), Http.Queries.Empty, ()).valid
//    )
//  }
//
//  test("encode") {
//    assertEquals(
//      obtained = url.encode(("foo", 42, "bar", 42.some, 3L)),
//      expected = (Chain("foo", "foo", "bar", "42"), Http.Queries.of("x" -> "bar", "y" -> "42", "z" -> "3"))
//    )
//    assertEquals(
//      obtained = url.encode(("foo", 42, "bar", none, 3L)),
//      expected = (Chain("foo", "foo", "bar", "42"), Http.Queries.of("x" -> "bar", "z" -> "3"))
//    )
//  }
//
//  test("encode: Url.Root") {
//    assertEquals(
//      obtained = Url.Root.encode(()),
//      expected = (Chain.empty, Http.Queries.Empty)
//    )
//  }
