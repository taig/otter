package io.taig.openapi.http

import cats.syntax.all.*
import io.taig.openapi.OpenApi
import io.taig.openapi.syntax.*
import io.taig.openapi.schema.schemas.*
import io.taig.openapi.http.schemas.*
import io.taig.openapi.http.syntax.*
import munit.FunSuite

final class SchemasTest extends FunSuite:
  test("error") {
    val schema = error("foo", int) orElse error("bar", string)

    assertEquals(
      obtained = schema.encode(Left(3)),
      expected = OpenApi.obj("type" := "foo", "hint" := None, "value" := 3)
    )
    assertEquals(
      obtained = schema.encode(Right("foobar")),
      expected = OpenApi.obj("type" := "bar", "hint" := None, "value" := "foobar")
    )

    assertEquals(
      obtained = schema.decode(OpenApi.obj("type" := "foo", "hint" := None, "value" := 3)),
      expected = Left(3).valid
    )
  }
