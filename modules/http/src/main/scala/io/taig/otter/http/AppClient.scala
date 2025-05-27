package io.taig.otter.http

import cats.ApplicativeThrow
import io.taig.otter.http.codec.PayloadDecoder
import io.taig.otter.http.codec.PayloadEncoder

object AppClient:
  def apply[F[_]: ApplicativeThrow, S[_]](
      decoder: PayloadDecoder[S],
      encoder: PayloadEncoder[S],
      debug: Boolean
  )(app: App[F, S]): Client[F, S] =
    val http = AppHttpClient(decoder, encoder, debug)(app)
    Client(http, decoder, encoder)
