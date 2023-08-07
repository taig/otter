package io.taig.otter.sample

import cats.effect.{IO, IOApp}
import io.taig.otter.circe.syntax.request
import io.taig.otter.circe.syntax.response
import io.taig.otter.http.*
import io.taig.otter.schema.schemas.*
import io.taig.otter.http.syntax.*

object SampleApp extends IOApp.Simple:
  override def run: IO[Unit] =
    val endpoint: Endpoint[User, Unit] = Endpoint(
      Request(method.get, Url.Root, Headers.Empty, request.json(schemas.user)).imap { case (_, _, user) => user }(
        user => ((), (), user)
      ),
      Response(Results(Result(code.ok)), Result(code.badRequest, response.json(violations)))
    )

    IO.println(endpoint)
