//package io.taig.otter.http
//
//import cats.data.Chain
//import cats.effect.IO
//import munit.{CatsEffectSuite, FunSuite}
//
//final class InputTest extends FunSuite:
//  test("encode"):
//
//    assertEquals(
//      obtained = Request(method.get, Url.Root, Headers.Empty, input.body.strict.empty).encode(()),
//      expected = Request(
//        method.get,
//        Chain.empty,
//        Http.Queries.Empty,
//        Http.Headers.Empty,
//        Request.Body.Singlepart.Strict.Empty
//      )
//    )
