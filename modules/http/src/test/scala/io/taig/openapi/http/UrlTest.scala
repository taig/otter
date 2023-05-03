//package io.taig.openapi.http
//
//import cats.data.Chain
//import cats.syntax.all.*
//import io.taig.openapi.{History, OpenApi}
//import munit.FunSuite
//import io.taig.openapi.schema.schemas.*
//import io.taig.openapi.http.syntax.*
//import io.taig.openapi.schema.{Violations, Void}
//import io.taig.openapi.validation.Constraint
//
//import java.util.UUID
//import scala.collection.immutable.VectorMap
//
//final class UrlTest extends FunSuite:
//  val url: Url[(String, Int, Long, Option[UUID], String)] =
//    __ / "foo" / parameter("a", string) / "bar" / parameter("b", int)
//      & query("x", long)
//      & query("y", uuid).optional
//      & query("z", string)
//
//  val zeroUuid = UUID.fromString("00000000-0000-0000-0000-000000000000")
//
//  test("matches") {
//    assertEquals(
//      obtained = url.matches(path = Chain.empty, queries = VectorMap.empty),
//      expected = false
//    )
//    assertEquals(
//      obtained = url.matches(
//        path = Chain(
//          OpenApi.fromString("foo"),
//          OpenApi.fromString("xxx"),
//          OpenApi.fromString("bar"),
//          OpenApi.fromInt(42)
//        ),
//        queries = VectorMap(
//          "x" -> "42",
//          "y" -> "not a UUID",
//          "z" -> "foobar"
//        )
//      ),
//      expected = true
//    )
//    assertEquals(
//      obtained = url.matches(
//        path = Chain(
//          OpenApi.fromString("foo"),
//          OpenApi.fromString("xxx"),
//          OpenApi.fromString("bar"),
//          OpenApi.fromInt(42)
//        ),
//        queries = VectorMap(
//          "x" -> "42",
//          "z" -> "foobar"
//        )
//      ),
//      expected = true
//    )
//    assertEquals(
//      obtained = url.matches(
//        path = Chain(
//          OpenApi.fromString("oof"),
//          OpenApi.fromString("xxx"),
//          OpenApi.fromString("bar"),
//          OpenApi.fromInt(42)
//        ),
//        queries = VectorMap(
//          "x" -> "42",
//          "z" -> "foobar"
//        )
//      ),
//      expected = false
//    )
//    assertEquals(
//      obtained = url.matches(
//        path = Chain(
//          OpenApi.fromString("foo"),
//          OpenApi.fromString("xxx"),
//          OpenApi.fromString("rab"),
//          OpenApi.fromInt(42)
//        ),
//        queries = VectorMap(
//          "x" -> "42",
//          "z" -> "foobar"
//        )
//      ),
//      expected = false
//    )
//    assertEquals(
//      obtained = url.matches(
//        path = Chain(
//          OpenApi.fromString("foo"),
//          OpenApi.fromString("bar")
//        ),
//        queries = VectorMap(
//          "x" -> "42",
//          "z" -> "foobar"
//        )
//      ),
//      expected = false
//    )
//    assertEquals(
//      obtained = url.matches(
//        path = Chain(
//          OpenApi.fromString("foo"),
//          OpenApi.fromString("xxx"),
//          OpenApi.fromString("bar"),
//          OpenApi.fromInt(42)
//        ),
//        queries = VectorMap(
//          "x" -> "42"
//        )
//      ),
//      expected = false
//    )
//  }
//
//  test("matches: Url.Root") {
//    assertEquals(
//      obtained = Url.Root.matches(path = Chain.empty, queries = VectorMap.empty),
//      expected = true
//    )
//    assertEquals(
//      obtained = Url.Root.matches(path = Chain.empty, queries = VectorMap("foo" -> "bar")),
//      expected = true
//    )
//    assertEquals(
//      obtained = Url.Root.matches(path = Chain(OpenApi.fromString("foobar")), queries = VectorMap.empty),
//      expected = false
//    )
//  }
//
//  test("decodeWithRemainders") {
//    assertEquals(
//      obtained = url.decodeWithRemainders(
//        path = Chain(
//          OpenApi.fromString("foo"),
//          OpenApi.fromString("xxx"),
//          OpenApi.fromString("bar"),
//          OpenApi.fromInt(42)
//        ),
//        queries = VectorMap("x" -> "42", "y" -> zeroUuid.toString, "z" -> "foobar")
//      ),
//      expected = (Chain.empty, VectorMap.empty, ("xxx", 42, 42L, zeroUuid.some, "foobar")).valid
//    )
//  }
//
//  test("decodeWithRemainders: query validation error") {
//    assertEquals(
//      obtained = (Url.Root & query("foo", int))
//        .decodeWithRemainders(path = Chain.empty, queries = VectorMap("foo" -> "42")),
//      expected = (Chain.empty, VectorMap.empty, 42).valid
//    )
//  }
//
//  test("decodeWithRemainders: Url.Empty") {
//    assertEquals(
//      obtained = Url.Root.decodeWithRemainders(path = Chain.empty, queries = VectorMap.empty),
//      expected = (Chain.empty, VectorMap.empty, Void).valid
//    )
//    assertEquals(
//      obtained = Url.Root.decodeWithRemainders(path = Chain.empty, queries = VectorMap("foo" -> "bar")),
//      expected = (Chain.empty, VectorMap("foo" -> "bar"), Void).valid
//    )
//    assertEquals(
//      obtained = Url.Root.decodeWithRemainders(path = Chain(OpenApi.fromString("foobar")), queries = VectorMap.empty),
//      expected = Violations
//        .oneNec(
//          History.Root / "path",
//          Constraint.text.equal(OpenApi.fromString("/")).toViolation(OpenApi.fromString("/foobar"))
//        )
//        .invalid
//    )
//  }
//
//  test("encode") {
//    val uuid = UUID.fromString("00000000-0000-0000-0000-000000000000")
//
//    assertEquals(
//      obtained = url.encode(("asdf", 42, 3L, uuid.some, "foobar")),
//      expected = (
//        Chain(
//          OpenApi.fromString("foo"),
//          OpenApi.fromString("asdf"),
//          OpenApi.fromString("bar"),
//          OpenApi.fromInt(42)
//        ),
//        VectorMap("x" -> "3", "y" -> uuid.toString, "z" -> "foobar")
//      )
//    )
//    assertEquals(
//      obtained = url.encode(("asdf", 42, 3L, none, "foobar")),
//      expected = (
//        Chain(
//          OpenApi.fromString("foo"),
//          OpenApi.fromString("asdf"),
//          OpenApi.fromString("bar"),
//          OpenApi.fromInt(42)
//        ),
//        VectorMap("x" -> "3", "z" -> "foobar")
//      )
//    )
//  }
//
//  test("encode: Url.Root") {
//    assertEquals(
//      obtained = Url.Root.encode(Void),
//      expected = (Chain.empty, VectorMap.empty)
//    )
//  }
