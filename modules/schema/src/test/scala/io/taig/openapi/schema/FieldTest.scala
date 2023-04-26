//package io.taig.openapi.schema
//
//import cats.syntax.all.*
//import io.taig.openapi.{History, OpenApi}
//import io.taig.openapi.schema.schemas.*
//import io.taig.validation.{Constraint, Violation}
//import munit.FunSuite
//
//final class FieldTest extends FunSuite:
//  test("decode") {
//    assertEquals(
//      obtained = field("foo", int).decode(OpenApi.obj("foo" -> OpenApi.fromInt(42))),
//      expected = (OpenApi.Object.Empty, 42).valid
//    )
//  }
//
//  test("decode: remainder") {
//    assertEquals(
//      obtained = field("foo", int).decode(
//        OpenApi.obj("foo" -> OpenApi.fromInt(42), "bar" -> OpenApi.fromBoolean(true))
//      ),
//      expected = (OpenApi.obj("bar" -> OpenApi.fromBoolean(true)), 42).valid
//    )
//  }
//
//  test("decode: violations") {
//    assertEquals(
//      obtained = field("foo", int).decode(OpenApi.Object.Empty),
//      expected = Violations
//        .oneNec(
//          History.Root / "foo",
//          Violation(Constraint("required", OpenApi.fromString("OpenApi.Primitive").some), OpenApi.Null)
//        )
//        .invalid
//    )
//  }
//
//  test("encode") {
//    assertEquals(
//      obtained = field("foo", int).encode(42, Product.Nulls.Show),
//      expected = OpenApi.obj("foo" -> OpenApi.fromInt(42))
//    )
//  }
//
//  test("encode: show nulls") {
//    assertEquals(
//      obtained = field("foo", int.optional).nulls.show.encode(42.some, Product.Nulls.Show),
//      expected = OpenApi.obj("foo" -> OpenApi.fromInt(42))
//    )
//    assertEquals(
//      obtained = field("foo", int.optional).nulls.show.encode(none, Product.Nulls.Show),
//      expected = OpenApi.obj("foo" -> OpenApi.Null)
//    )
//  }
//
//  test("encode: hide nulls") {
//    assertEquals(
//      obtained = field("foo", int.optional).nulls.hide.encode(42.some, Product.Nulls.Show),
//      expected = OpenApi.obj("foo" -> OpenApi.fromInt(42))
//    )
//    assertEquals(
//      obtained = field("foo", int.optional).nulls.hide.encode(none, Product.Nulls.Show),
//      expected = OpenApi.Object.Empty
//    )
//  }
//
//  test("encode: inherit nulls") {
//    assertEquals(
//      obtained = field("foo", int.optional).nulls.inherit.encode(none, Product.Nulls.Show),
//      expected = OpenApi.obj("foo" -> OpenApi.Null)
//    )
//    assertEquals(
//      obtained = field("foo", int.optional).nulls.inherit.encode(none, Product.Nulls.Hide),
//      expected = OpenApi.Object.Empty
//    )
//  }
