package io.taig.otter.sample.api

import cats.MonadThrow
import cats.syntax.all.*
import io.taig.otter.http.Route
import io.taig.otter.sample.api.schema.SelfApiSchema
import io.taig.otter.sample.api.schema.SessionApiSchema
import io.taig.otter.sample.api.schema.UserApiSchema

abstract class EndpointImplementation[F[_]](using F: MonadThrow[F]):
  final def apply[R <: Role, I, O](endpoint: RoleEndpoint[R, I, O])(
      f: (SelfApiSchema[R], I) => F[O]
  ): RoleRoute[F, I, O] = Route(
    endpoint = endpoint.toAuthenticatedEndpoint,
    implementation = authentication =>
      authentication.session
        .match
          case Some(session) =>
            for
              user <- findUser(session).flatMap(_.liftTo[F](AuthenticationApiSchema.Error.UserUnknown))
              _ <- F.raiseWhen(!endpoint.role.toSet.contains_(Role.from(user)))(AuthenticationApiSchema.Error.Forbidden)
              response <- f(user.asInstanceOf[SelfApiSchema[R]], authentication.payload)
            yield response
          case None if endpoint.role.toSet.contains_(Role.Guest) =>
            f(().asInstanceOf[SelfApiSchema[R]], authentication.payload)
          case None => F.raiseError(AuthenticationApiSchema.Error.Forbidden)
        .attemptNarrow[AuthenticationApiSchema.Error]
  )

  def findUser(session: SessionApiSchema): F[Option[UserApiSchema]]
