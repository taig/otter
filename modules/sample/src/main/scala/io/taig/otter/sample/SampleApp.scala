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
      request(method.post, Url.Root, request.of(io.taig.otter.circe.request.openapi, schemas.user)),
      Response(
        Results(result(code.ok, response.of(io.taig.otter.circe.response.openapi, schemas.user))),
        result(code.badRequest, response.of(io.taig.otter.circe.response.openapi, violations))
      )
    )

    val route = Route(endpoint, IO.pure)

    val app = App(
      route.toRoutes,
      Response(
        Results(result(code.notFound)),
        result(code.badRequest, response.of(io.taig.otter.circe.response.openapi, violations))
      ),
      Response(
        Results(result(code.internalServerError)),
        result(code.badRequest, response.of(io.taig.otter.circe.response.openapi, violations))
      )
    )

    given LoggerFactory[IO] = Slf4jFactory.create[IO]

    val server = new Http4sHttpServer[IO]

    server.start(app)
