package io.taig.openapi.http4s

import cats.{Monad, MonadThrow}
import cats.effect.Concurrent
import cats.syntax.all.*
import io.taig.openapi.http.*
import org.http4s.HttpApp

final class Http4sClient[F[+_]](app: HttpApp[F])(using F: Concurrent[F]) extends Client[F]:
  override def submitRaw[I, O](endpoint: Endpoint[I, O], request: Request[F]): F[Response] =
    F.fromEither(toHttp4sRequest[F](request)).flatMap(app.run).flatMap(fromHttp4sResponse[F])

object Http4sClient:
  def apply[F[+_]: Concurrent](app: HttpApp[F]): Client[F] = new Http4sClient[F](app)
