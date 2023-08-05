package io.taig.otter.http

import cats.effect.Concurrent
import cats.syntax.all.*
import fs2.Stream

object Fs2HttpDecoder:
  def decode[F[_]: Concurrent](body: Request.Body[?], data: Stream[F, Byte]): F[Http.Request.Body] = body match
    case _: Request.Body.Singlepart.Strict[?] => data.compile.to(Array).map(Http.Request.Body.Singlepart.Strict.apply)
    case _: Request.Body.Singlepart.Streaming[?] => Fs2Stream(data).map(Http.Request.Body.Singlepart.Streaming.apply)
