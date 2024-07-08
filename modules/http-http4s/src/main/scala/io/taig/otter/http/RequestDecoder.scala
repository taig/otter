package io.taig.otter.http

import io.taig.otter.http.Plain.*
import org.http4s.Header as Http4sHeader
import org.http4s.Headers as Http4sHeaders
import org.http4s.Request as Http4sRequest
import io.taig.otter.Decoder

object RequestDecoder:
  def apply[F[_], A](request: Request[A], value: Http4sRequest[F]): Decoder.Result[Any, Option[A]] = ???
