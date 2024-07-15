package io.taig.otter.http

import cats.effect.Concurrent
import cats.syntax.all.*
import io.taig.otter.Decoder
import org.http4s.Entity as Http4sEntity
import org.http4s.Headers as Http4sHeaders
import org.http4s.Entity.Streamed
import org.http4s.Entity.Strict
import org.http4s.Entity.Empty
import cats.data.Validated

object RequestBodyDecoder:
  def apply[F[_]: Concurrent, A](body: Request.Body[A], values: Http4sHeaders, value: Http4sEntity[F]): F[Decoder.Result[Option[String], A]] =
    body match
      case Request.Body.Singlepart.Strict.Empty =>
        value match
          case Http4sEntity.Streamed(body, length) => ??? // TODO this is bad but should I just consume the stream?
          case Http4sEntity.Strict(bytes)          => ??? // TODO this is an error
          case Http4sEntity.Empty                  => ().valid.pure
      case Request.Body.Singlepart.Strict.Binary =>
        value match
          case Http4sEntity.Streamed(body, _) => body.compile.to(Array).map(_.valid)
          case Http4sEntity.Strict(bytes)     => bytes.toArray.valid.pure
          case Http4sEntity.Empty             => ??? // TODO this is an error
      case Request.Body.Singlepart.Strict.Optional(self) => ???
      case Request.Body.Singlepart.Strict.Apply(headers, parse, decoder, schema) =>
        value match
          case Http4sEntity.Streamed(body, _) =>
            HeadersDecoder(headers, values) match
              case Validated.Valid(a) =>
                body.compile.to(Array).map(parse(a, _)).rethrow.map(decoder(schema, _))
            // body.compile.to(Array).map(parser).map(decoder.apply(schema, _))
            ???
          case Http4sEntity.Strict(bytes) =>
            // val a = parser(bytes.toArray)
            // decoder.apply(schema, a).pure
            ???
          case Http4sEntity.Empty => ??? // TODO this is an error
