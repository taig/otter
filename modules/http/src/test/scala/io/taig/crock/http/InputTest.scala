package io.taig.crock.http

import cats.effect.IO
import cats.data.Chain
import munit.{CatsEffectSuite, FunSuite}

final class InputTest extends FunSuite:
  test("encode"):

    assertEquals(
      obtained = Input(method.get, Url.Root, Headers.Empty, input.body.strict.empty).encode(()),
      expected = Request(
        method.get,
        Chain.empty,
        Http.Queries.Empty,
        Http.Headers.Empty,
        Request.Body.Singlepart.Strict.Empty
      )
    )
