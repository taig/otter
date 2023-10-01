package io.taig.otter.sample.api.endpoints

import cats.data.Chain
import io.taig.otter.http.{Endpoint as OtterEndpoint, Request, Response}
import io.taig.otter.sample.api.{headers, schemas, Role}

final case class Endpoint[R <: Role, I, O](
    role: R,
    toAuthenticatedEndpoint: OtterEndpoint[Authentication[I], Either[Authentication.Error, O]]
)

extension [I, O](self: OtterEndpoint[I, O])
  def role[R <: Role](role: R): Endpoint[R, I, O] = Endpoint(
    role,
    self
      .request { request =>
        // TODO provide better syntax
        request
          .zip(headers.authorizationBearerUuid.optional.toHeaders)
          .imap { case (payload, session) =>
            Authentication(session, payload)
          }(authentication => (authentication.payload, authentication.session))
      }
      .response(_.results(schemas.authentication.error.orElse))
  )
