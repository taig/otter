package io.taig.openapi.schema

import cats.syntax.all.*
import io.taig.openapi.OpenApi
import munit.FunSuite

final class PrimitiveTest extends FunSuite:
  test("Primitive") {
    assertEquals(
      obtained = Primitive(Type.String).encode("foobar"),
      expected = OpenApi.fromString("foobar")
    )

    assertEquals(
      obtained = Primitive(Type.String).decode(OpenApi.fromString("foobar")),
      expected = "foobar".valid
    )
  }
