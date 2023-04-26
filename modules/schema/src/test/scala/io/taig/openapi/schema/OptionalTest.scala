//package io.taig.openapi.schema
//
//import cats.syntax.all.*
//import io.taig.openapi.OpenApi
//import io.taig.openapi.schema.schemas.*
//import munit.FunSuite
//
//final class OptionalTest extends FunSuite:
//  test("decode: primitive") {
//    assertEquals(
//      obtained = int.optional.decode(OpenApi.fromInt(42)),
//      expected = Some(42).valid
//    )
//    assertEquals(
//      obtained = int.optional.decode(OpenApi.Null),
//      expected = None.valid
//    )
//  }
//
//  test("encode: primitive") {
//    assertEquals(
//      obtained = int.optional.encode(Some(42)),
//      expected = OpenApi.fromInt(42)
//    )
//    assertEquals(
//      obtained = int.optional.encode(None),
//      expected = OpenApi.Null
//    )
//  }
