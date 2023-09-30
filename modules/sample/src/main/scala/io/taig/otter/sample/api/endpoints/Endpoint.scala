package io.taig.otter.sample.api.endpoints

import cats.data.Chain
import io.taig.otter.http.{Endpoint as OtterEndpoint, Request, Response}
import io.taig.otter.sample.api.Roles
import io.taig.otter.sample.api.headers
import io.taig.otter.sample.api.schemas

import java.util.UUID

final case class Endpoint[R, I, O](
    roles: Roles[R],
    toUnauthenticatedEndpoint: OtterEndpoint[Authentication[UUID, I], Either[Authentication.Error, O]]
):
  def tags: Chain[String] = toUnauthenticatedEndpoint.tags
  def tags(f: Chain[String] => Chain[String]): Endpoint[R, I, O] =
    copy(toUnauthenticatedEndpoint = toUnauthenticatedEndpoint.tags(f))
  def tags(values: Chain[String]): Endpoint[R, I, O] = tags(_ => values)
  def tags(values: String*): Endpoint[R, I, O] = tags(Chain.fromSeq(values))

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
