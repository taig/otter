package io.taig.otter.http

import cats.syntax.all.*
import io.taig.otter.http.header.Accept
import cats.MonadThrow
import io.taig.otter.Violations

final case class Route[F[_], I, O](endpoint: Endpoint[I, O], implementation: I => F[O]):
  def apply(accept: Option[Accept.Result], request: Http.Request, onError: Throwable => F[Unit])(using
      MonadThrow[F]
  ): F[Http.Response] = endpoint.request
    .decode(request)
    .traverse(implementation)
    .map(endpoint.response.encode(accept, _))
  // .handleErrorWith: throwable =>
  //   onError(throwable) *>
  //     endpoint.response.failure.encode(accept, Request.Result.Success(())).pure[F]

  def :+(endpoint: Route[F, ?, ?]): Routes[F] = toRoutes :+ endpoint

  def +:(endpoint: Route[F, ?, ?]): Routes[F] = endpoint +: toRoutes

  def toRoutes: Routes[F] = Routes.one(this)

object Route:
  final case class Error(tpe: Route.Error.Type, violations: Violations)

  object Error:
    enum Type:
      case ContentNegotiationFailed
      case MediaTypesUnsupported
      case ValidationViolations

    def validationViolations(violations: Violations): Error = Error(Type.ValidationViolations, violations)
