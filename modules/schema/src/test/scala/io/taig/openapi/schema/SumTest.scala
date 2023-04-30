package io.taig.openapi.schema

import io.taig.openapi.OpenApi
import io.taig.openapi.schema.schemas.*
import io.taig.openapi
import munit.FunSuite

final class SumTest extends FunSuite:
  enum Foo:
    case Bar(name: String)

  test("as: enum 1") {
    val bar = field("name", string).toProduct.as[Foo.Bar]
    val foo = branch("bar", bar).toSum.as[Foo]

    assertEquals(
      obtained = foo.encode(Foo.Bar("foobar")),
      expected = OpenApi.obj(
        "type" -> OpenApi.fromString("bar"),
        "value" -> OpenApi.obj("name" -> OpenApi.fromString("foobar"))
      )
    )
  }
