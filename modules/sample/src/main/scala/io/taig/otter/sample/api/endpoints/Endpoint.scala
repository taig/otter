package io.taig.otter.sample.api.endpoints

import io.taig.otter.http.{Endpoint as OtterEndpoint, Request, Response}
import io.taig.otter.sample.api.Roles
import io.taig.otter.sample.api.headers
import io.taig.otter.sample.api.schemas

import java.util.UUID

final case class Endpoint[R, I, O](
    roles: Roles[R],
    toUnauthenticatedEndpoint: OtterEndpoint[Authentication[UUID, I], Either[Authentication.Error, O]]
)

object Endpoint:
  def apply[R, I, O](roles: Roles[R], request: Request[I], response: Response[O]): Endpoint[R, I, O] = Endpoint(
    roles,
    OtterEndpoint(request, response)
      .modifyRequest { request =>
        // TODO provide better syntax
        request
          .zip(headers.authorizationBearerUuid.optional.toHeaders)
          .imap { case (payload, session) =>
            Authentication(session, payload)
          }(authentication => (authentication.payload, authentication.self))
      }
      .modifyResponse(_.modifyResults(schemas.authentication.error.orElse))
  )
