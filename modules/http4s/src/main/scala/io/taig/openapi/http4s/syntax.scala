package io.taig.openapi.http4s

import cats.effect.Concurrent
import io.circe.Json
import io.taig.openapi.schema.Schema
import org.http4s.{EntityDecoder, Message}

object syntax:
  extension [F[_]](message: Message[F])
    def asJsonCodec[A](schema: Schema[A])(implicit F: Concurrent[F], decoder: EntityDecoder[F, Json]): F[A] =
      message.as(F, jsonCodecOf[F, A](decoder, schema))
