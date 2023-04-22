package io.taig.openapi.http

import cats.data.Chain
import cats.syntax.all.*
import io.taig.openapi.OpenApi
import io.taig.openapi.http.schemas.*
import io.taig.openapi.http.syntax.*
import io.taig.openapi.schema.schemas.*
import munit.FunSuite

final class OutputTest extends FunSuite:
  test("response code overloading") {
    val results = result(code.ok, int) + result(code.ok, string)

    assertEquals(
      obtained = results.decode(Response(code.ok, Chain.empty, OpenApi.fromInt(3).some)),
      expected = 3.asLeft.some.valid
    )

    assertEquals(
      obtained = results.decode(Response(code.ok, Chain.empty, OpenApi.fromString("foobar").some)),
      expected = "foobar".asRight.some.valid
    )
  }
