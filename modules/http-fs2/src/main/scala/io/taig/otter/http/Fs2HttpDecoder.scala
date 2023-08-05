package io.taig.otter.http

import cats.data.Validated
import cats.effect.Concurrent
import cats.syntax.all.*
import fs2.Stream
import io.taig.otter.schema.{Decoder, Violations}

//object Fs2HttpDecoder:
//  def body[F[_]: Concurrent]: Decoder[F, Request.Body, Stream[F, Byte]] = new Decoder:
//    override def decode[A](body: Request.Body[A], data: Stream[F, Byte]): F[Validated[Violations, A]] = body match
//      case Request.Body.Singlepart.Strict.Empty => ().valid.pure
//      case Request.Body.Singlepart.Strict.Bytes => data.compile.to(Array).map(_.valid)
//      case Request.Body.Singlepart.Strict.Validate(self, validation, _) =>
//        decode(self, data).map(_.andThen(validation(_).leftMap(Violations.root)))
//      case Request.Body.Singlepart.Streaming.Empty => ().valid.pure
//      case Request.Body.Singlepart.Streaming.Bytes => Fs2Stream(data).map(_.valid)
//      case Request.Body.Singlepart.Streaming.Validate(self, validation, _) =>
//        decode(self, data).map(_.andThen(validation(_).leftMap(Violations.root)))
