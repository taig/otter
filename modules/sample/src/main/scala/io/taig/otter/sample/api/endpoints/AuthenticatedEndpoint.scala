package io.taig.otter.sample.api.endpoints

import io.taig.otter.http.Endpoint
import io.taig.otter.sample.api.{headers, Authentication, Role}

final case class AuthenticatedEndpoint[R <: Role, I, O](
    role: R,
    toAuthenticatedEndpoint: Endpoint[Authentication[I], Either[Authentication.Error, O]]
)

extension [I, O](self: Endpoint[I, O])
  def role[R <: Role](role: R): AuthenticatedEndpoint[R, I, O] = AuthenticatedEndpoint(
    role,
    self
      .request { request =>
        // TODO provide better syntax
        request
          .zip(headers.session.optional.toHeaders)
          .imap { case (payload, session) =>
            Authentication(session, payload)
          }(authentication => (authentication.payload, authentication.session))
      }
      .response(_.results(Authentication.codec.orElse))
  )
