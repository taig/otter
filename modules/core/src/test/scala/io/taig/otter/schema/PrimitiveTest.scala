//package io.taig.otter.schema
//
//import cats.syntax.all.*
//import io.taig.otter.schemas.*
//import io.taig.otter.validation.{validations, Constraint, Violation}
//import munit.FunSuite
//
//import java.util.UUID
//
//final class PrimitiveTest extends FunSuite:
//  test("decode"):
//    assertEquals(obtained = int.decode(OpenApi.Integer(42)), expected = 42.valid)
//
//  test("decode: null"):
//    assertEquals(
//      obtained = string.decode(OpenApi.Null),
//      expected = Violations.rootNec(Violation.required).invalid
//    )
//
//  test("decode: type mismatch"):
//    val value = OpenApi.Integer(42)
//
//    assertEquals(
//      obtained = string.decode(value),
//      expected = Violations.rootNec(Violation.tpe("string", "number")).invalid
//    )
//
//  test("decode: uuid"):
//    val value = UUID.fromString("b8db8a93-9aef-43ee-90a9-68cf72069867")
//    assertEquals(obtained = uuid.decode(OpenApi.String(value.toString)), expected = value.valid)
//
//  test("encode"):
//    assertEquals(obtained = int.encode(42), expected = OpenApi.Integer(42).some)
//
//  test("encode: UUID"):
//    val value = UUID.fromString("b8db8a93-9aef-43ee-90a9-68cf72069867")
//    assertEquals(obtained = uuid.encode(value), expected = OpenApi.String(value.toString).some)
//
//  test("validate"):
//    val foobar = string.validate(validations.equal("foobar"))
//
//    assertEquals(
//      obtained = foobar.decode(OpenApi.String("foobar")),
//      expected = "foobar".valid
//    )
//
//    assertEquals(
//      obtained = foobar.decode(OpenApi.String("foo")),
//      expected = Violations
//        .rootNec(Violation(Constraint.Equals("foobar"), OpenApi.String("foo")))
//        .invalid
//    )
