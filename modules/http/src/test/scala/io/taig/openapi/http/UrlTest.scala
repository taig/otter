package io.taig.openapi.http

import cats.data.Chain
import io.taig.openapi.OpenApi
import munit.FunSuite
import io.taig.openapi.schema.schemas.*
import io.taig.openapi.http.syntax.*
import io.taig.openapi.schema.Void

import java.util.UUID
import scala.collection.immutable.VectorMap

final class UrlTest extends FunSuite:
  val url: Url[(String, Int, Long, Option[UUID], String)] =
    __ / "foo" / parameter("a", string) / "bar" / parameter("b", int)
      & query("x", long)
      & query("y", uuid).optional
      & query("z", string)

  test("matches") {
    assertEquals(
      obtained = url.matches(path = Chain.empty, queries = VectorMap.empty),
      expected = false
    )
    assertEquals(
      obtained = url.matches(
        path = Chain(
          OpenApi.fromString("foo"),
          OpenApi.fromString("xxx"),
          OpenApi.fromString("bar"),
          OpenApi.fromInt(42)
        ),
        queries = VectorMap(
          "x" -> OpenApi.fromLong(42L),
          "y" -> OpenApi.fromString("not a UUID"),
          "z" -> OpenApi.fromString("foobar")
        )
      ),
      expected = true
    )
    assertEquals(
      obtained = url.matches(
        path = Chain(
          OpenApi.fromString("foo"),
          OpenApi.fromString("xxx"),
          OpenApi.fromString("bar"),
          OpenApi.fromInt(42)
        ),
        queries = VectorMap(
          "x" -> OpenApi.fromLong(42L),
          "z" -> OpenApi.fromString("foobar")
        )
      ),
      expected = true
    )
    assertEquals(
      obtained = url.matches(
        path = Chain(
          OpenApi.fromString("oof"),
          OpenApi.fromString("xxx"),
          OpenApi.fromString("bar"),
          OpenApi.fromInt(42)
        ),
        queries = VectorMap(
          "x" -> OpenApi.fromLong(42L),
          "z" -> OpenApi.fromString("foobar")
        )
      ),
      expected = false
    )
    assertEquals(
      obtained = url.matches(
        path = Chain(
          OpenApi.fromString("foo"),
          OpenApi.fromString("xxx"),
          OpenApi.fromString("rab"),
          OpenApi.fromInt(42)
        ),
        queries = VectorMap(
          "x" -> OpenApi.fromLong(42L),
          "z" -> OpenApi.fromString("foobar")
        )
      ),
      expected = false
    )
    assertEquals(
      obtained = url.matches(
        path = Chain(
          OpenApi.fromString("foo"),
          OpenApi.fromString("bar")
        ),
        queries = VectorMap(
          "x" -> OpenApi.fromLong(42L),
          "z" -> OpenApi.fromString("foobar")
        )
      ),
      expected = false
    )
    assertEquals(
      obtained = url.matches(
        path = Chain(
          OpenApi.fromString("foo"),
          OpenApi.fromString("xxx"),
          OpenApi.fromString("bar"),
          OpenApi.fromInt(42)
        ),
        queries = VectorMap(
          "x" -> OpenApi.fromLong(42L)
        )
      ),
      expected = false
    )
  }

  test("matches: Url.Root") {
    assertEquals(
      obtained = Url.Root.matches(path = Chain.empty, queries = VectorMap.empty),
      expected = true
    )
    assertEquals(
      obtained = Url.Root.matches(path = Chain.empty, queries = VectorMap("foo" -> OpenApi.fromString("bar"))),
      expected = true
    )
    assertEquals(
      obtained = Url.Root.matches(path = Chain(OpenApi.fromString("foobar")), queries = VectorMap.empty),
      expected = false
    )
  }
