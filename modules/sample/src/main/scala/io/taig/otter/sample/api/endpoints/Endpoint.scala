package io.taig.otter.sample.api.endpoints

import io.taig.otter.http.{Endpoint as OtterEndpoint, Request, Response}
import io.taig.otter.sample.api.{headers, schemas, Role}

type Endpoint[R <: Role, I, O] = OtterEndpoint[Authentication[I], Either[Authentication.Error, O]]

object Endpoint:
  def apply[R <: Role, I, O](request: Request[I], response: Response[O]): Endpoint[R, I, O] =
    OtterEndpoint(request, response)
      .request { request =>
        // TODO provide better syntax
        request
          .zip(headers.authorizationBearerUuid.optional.toHeaders)
          .imap { case (payload, session) =>
            Authentication(session, payload)
          }(authentication => (authentication.payload, authentication.session))
      }
      .response(_.modifyResults(schemas.authentication.error.orElse))
