package io.taig.openapi.schema

import cats.syntax.all.*
import io.taig.openapi.OpenApi
import io.taig.openapi.schema.schemas.*
import io.taig.validation.{Constraint, Violation}
import munit.FunSuite

import java.util.UUID

final class PrimitiveTest extends FunSuite:
  test("decode"):
      assertEquals(obtained = int.decode(OpenApi.fromInt(42)), expected = 42.valid)

  test("decode: null"):
      assertEquals(
        obtained = string.decode(OpenApi.Null),
        expected = Violations
          .rootNec(
            Violation(Constraint.withReference("required", OpenApi.fromString("OpenApi.Primitive")), OpenApi.Null)
          )
          .invalid
      )

  test("decode: null (with default)"):
      assertEquals(
        obtained = string.default.as("foobar").decode(OpenApi.Null),
        expected = "foobar".valid
      )

  test("decode: type mismatch"):
      val value = OpenApi.fromInt(42)
      assertEquals(
        obtained = string.decode(value),
        expected = Violations
          .rootNec(
            Violation(Constraint.withReference("type", OpenApi.fromString("String")), value)
          )
          .invalid
      )

  test("uuid"):
      val value = UUID.randomUUID()
      assertEquals(obtained = uuid.decode(OpenApi.fromString(value.toString)), expected = value.valid)

  test("encode"):
      assertEquals(obtained = int.encode(42), expected = OpenApi.fromInt(42))

  test("encode: UUID"):
      val value = UUID.randomUUID()
      assertEquals(obtained = uuid.encode(value), expected = OpenApi.fromString(value.toString))
