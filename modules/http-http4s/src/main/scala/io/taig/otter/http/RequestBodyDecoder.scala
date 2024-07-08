package io.taig.otter.http

import io.taig.otter.Decoder
import io.taig.otter.http.Plain.*
import io.taig.otter.http as Http
import org.http4s.Entity as Http4sEntity

object RequestBodyDecoder:
  def apply[F[_], A](body: Request.Body[A], value: Http4sEntity[F]): Decoder.Result[Any, A] = body match
    case Http.Request.Body.Singlepart.Strict(schema) =>
      value match
        case Http4sEntity.Empty             => ???
        case Http4sEntity.Strict(bytes)     => ???
        case Http4sEntity.Streamed(body, _) => ???
