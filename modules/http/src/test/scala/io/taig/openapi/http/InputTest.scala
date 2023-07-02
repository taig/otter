package io.taig.openapi.http

import cats.effect.IO
import cats.data.Chain
import io.taig.openapi.http.syntax.*
import munit.CatsEffectSuite

final class InputTest extends CatsEffectSuite:
  test("encode"):
    for obtained <- Input(method.get, Url.Root, Headers.Empty, input.body.strict.empty).encode[IO](())
    yield {
      assertEquals(
        obtained,
        expected = Request(
          method.get,
          Chain.empty,
          Http.Queries.Empty,
          Http.Headers.Empty,
          Request.Body.Singlepart(Stream.Empty)
        )
      )
    }
