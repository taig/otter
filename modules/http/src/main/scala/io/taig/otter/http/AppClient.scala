package io.taig.otter.http

import cats.ApplicativeThrow
import io.taig.otter.+
import io.taig.otter.http.codec.PayloadDecoder
import io.taig.otter.http.codec.PayloadEncoder

object AppClient:
  def apply[F[_]: ApplicativeThrow, S[_], T[_], U[_]](
      decoder: PayloadDecoder[S + T + U],
      encoder: PayloadEncoder[S + T + U],
      debug: Boolean
  )(app: App[F, S, T, U]): Client[F, S, T, U] =
    val http = AppHttpClient(decoder, encoder, debug)(app)
    Client(http, decoder, encoder)
