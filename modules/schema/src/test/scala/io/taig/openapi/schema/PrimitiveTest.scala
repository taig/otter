package io.taig.openapi.schema

import cats.syntax.all.*
import io.taig.openapi.OpenApi
import io.taig.openapi.schema.schemas.*
import io.taig.validation.{validations, Constraint, Violation}
import munit.FunSuite

import java.util.UUID

final class PrimitiveTest extends FunSuite:
  test("decode") {
    assertEquals(obtained = int.decode(OpenApi.fromInt(42)), expected = 42.valid)
  }

  test("decode: null") {
    assertEquals(
      obtained = string.decode(OpenApi.Null),
      expected = Violations
        .rootNec(
          Violation(Constraint.withReference("type", OpenApi.fromString("OpenApi.Primitive")), OpenApi.Null)
        )
        .invalid
    )
  }

  test("decode: type mismatch") {
    val value = OpenApi.fromInt(42)

    assertEquals(
      obtained = string.decode(value),
      expected = Violations
        .rootNec(
          Violation(Constraint.withReference("type", OpenApi.fromString("String")), value)
        )
        .invalid
    )
  }

  test("decode: uuid") {
    val value = UUID.randomUUID()

    assertEquals(obtained = uuid.decode(OpenApi.fromString(value.toString)), expected = value.valid)
  }

  test("encode") {
    assertEquals(obtained = int.encode(42), expected = OpenApi.fromInt(42))
  }

  test("encode: UUID") {
    val value = UUID.randomUUID()
    assertEquals(obtained = uuid.encode(value), expected = OpenApi.fromString(value.toString))
  }

  test("validate") {
    val foobar = string.validate(validations.text.equal("foobar"))

    assertEquals(
      obtained = foobar.decode(OpenApi.fromString("foobar")),
      expected = "foobar".valid
    )

    assertEquals(
      obtained = foobar.decode(OpenApi.fromString("foo")),
      expected = Violations
        .rootNec(
          Violation(Constraint.withReference("text.equal", OpenApi.fromString("foobar")), OpenApi.fromString("foo"))
        )
        .invalid
    )
  }
