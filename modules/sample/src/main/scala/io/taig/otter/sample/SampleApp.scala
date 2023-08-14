package io.taig.otter.sample

import cats.effect.{IO, IOApp}
import io.taig.otter.http.*
import io.taig.otter.schema.schemas.*
import io.taig.otter.http.syntax.*
import io.taig.otter.http4s.Http4sHttpServer
import org.typelevel.log4cats.LoggerFactory
import org.typelevel.log4cats.slf4j.Slf4jFactory

object SampleApp extends IOApp.Simple:
  override def run: IO[Unit] =
    val endpoint: Endpoint[User, User] = Endpoint(
      Request(method.post, Url.Root, Headers.Empty, io.taig.otter.circe.request.json(schemas.user))
        .imap { case (_, _, user) => user }(user => ((), (), user)),
      Response(
        Results(Result(code.ok, io.taig.otter.circe.response.json(schemas.user))),
        Result(code.badRequest, io.taig.otter.circe.response.json(violations))
      )
    )

    val route = Route(endpoint, IO.pure)

    val app = App(
      route.toRoutes,
      Response(
        Results(Result(code.notFound, response.empty)),
        Result(code.badRequest, io.taig.otter.circe.response.json(violations))
      ),
      Response(
        Results(Result(code.internalServerError, response.empty)),
        Result(code.badRequest, io.taig.otter.circe.response.json(violations))
      )
    )

    given LoggerFactory[IO] = Slf4jFactory.create[IO]

    val server = new Http4sHttpServer[IO]

    server.start(app)
