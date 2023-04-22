package io.taig.openapi.schema

import cats.Eval
import cats.syntax.all.*
import io.taig.openapi.OpenApi
import io.taig.openapi.schema.schemas.*
import io.taig.openapi.schema.syntax.*
import io.taig.openapi.syntax.*
import munit.FunSuite

final class SumTest extends FunSuite:
  test("of") {
    val product = field("type", string.const("foo")) *> field("value", string)
    val sum = Sum.of(branch("foo", string), Eval.now(product))

    assertEquals(
      obtained = sum.encode("bar"),
      expected = OpenApi.obj("type" := "foo", "value" := "bar")
    )

    assertEquals(
      obtained = sum.decode(OpenApi.obj("type" := "foo", "value" := "bar")),
      expected = "bar".valid
    )
  }
