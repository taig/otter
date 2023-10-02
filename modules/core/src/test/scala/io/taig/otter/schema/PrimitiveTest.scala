package io.taig.otter.codec

import cats.syntax.all.*
import io.taig.otter.Data
import io.taig.otter.codecs.*
import io.taig.otter.validation.{validations, Constraint, Violation, Violations}
import munit.FunSuite

import java.util.UUID

final class PrimitiveTest extends FunSuite:
  test("decode"):
    assertEquals(obtained = int.decode(Data.Number(42)), expected = 42.valid)

  test("decode: null"):
    assertEquals(
      obtained = string.decode(Data.Null),
      expected = Violations.rootNec(Violation.required).invalid
    )

  test("decode: type mismatch"):
    val value = Data.Number(42)

    assertEquals(
      obtained = string.decode(value),
      expected = Violations.rootNec(Violation.tpe("string", "number")).invalid
    )

  test("decode: uuid"):
    val value = UUID.fromString("b8db8a93-9aef-43ee-90a9-68cf72069867")
    assertEquals(obtained = uuid.decode(Data.String(value.toString)), expected = value.valid)

  test("encode"):
    assertEquals(obtained = int.encode(42), expected = Data.Number(42))

  test("encode: UUID"):
    val value = UUID.fromString("b8db8a93-9aef-43ee-90a9-68cf72069867")
    assertEquals(obtained = uuid.encode(value), expected = Data.String(value.toString))

  test("validate"):
    val foobar = string.validate(validations.equal("foobar"))

    assertEquals(
      obtained = foobar.decode(Data.String("foobar")),
      expected = "foobar".valid
    )

    assertEquals(
      obtained = foobar.decode(Data.String("foo")),
      expected = Violations
        .rootNec(Violation(Constraint.Equals("foobar"), Data.String("foo")))
        .invalid
    )
