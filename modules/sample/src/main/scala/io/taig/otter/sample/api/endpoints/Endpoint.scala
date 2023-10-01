package io.taig.otter.sample.api.endpoints

import cats.data.Chain
import io.taig.otter.http.{Endpoint as OtterEndpoint, EndpointLike, Request, Response}
import io.taig.otter.sample.api.Roles
import io.taig.otter.sample.api.headers
import io.taig.otter.sample.api.schemas

import java.util.UUID

final case class Endpoint[R, I, O](
    roles: Roles[R],
    toAuthenticatedEndpoint: Endpoint.Authenticated[I, O]
) extends EndpointLike[Endpoint[R, I, O]]:
  def endpoint[T, U](f: Endpoint.Authenticated[I, O] => Endpoint.Authenticated[T, U]): Endpoint[R, T, U] =
    copy(toAuthenticatedEndpoint = f(toAuthenticatedEndpoint))
  def endpoint[T, U](value: Endpoint.Authenticated[T, U]): Endpoint[R, T, U] = endpoint(_ => value)
  override def description: Option[String] = toAuthenticatedEndpoint.description
  override def description(f: Option[String] => Option[String]): Endpoint[R, I, O] =
    endpoint(toAuthenticatedEndpoint.description(f))
  override def hidden: Boolean = toAuthenticatedEndpoint.hidden
  override def hidden(f: Boolean => Boolean): Endpoint[R, I, O] =
    endpoint(toAuthenticatedEndpoint.hidden(f))
  override def operationId: Option[String] = toAuthenticatedEndpoint.operationId
  override def operationId(f: Option[String] => Option[String]): Endpoint[R, I, O] =
    endpoint(toAuthenticatedEndpoint.operationId(f))
  override def summary: Option[String] = toAuthenticatedEndpoint.summary
  override def summary(f: Option[String] => Option[String]): Endpoint[R, I, O] =
    endpoint(toAuthenticatedEndpoint.summary(f))
  override def tags: Chain[String] = toAuthenticatedEndpoint.tags
  override def tags(f: Chain[String] => Chain[String]): Endpoint[R, I, O] =
    endpoint(toAuthenticatedEndpoint.tags(f))

object Endpoint:
  type Authenticated[I, O] = OtterEndpoint[Authentication[UUID, I], Either[Authentication.Error, O]]

  def apply[R, I, O](roles: Roles[R], request: Request[I], response: Response[O]): Endpoint[R, I, O] =
    Endpoint(
      roles,
      OtterEndpoint(request, response)
        .request { request =>
          // TODO provide better syntax
          request
            .zip(headers.authorizationBearerUuid.optional.toHeaders)
            .imap { case (payload, session) =>
              Authentication(session, payload)
            }(authentication => (authentication.payload, authentication.self))
        }
        .response(_.modifyResults(schemas.authentication.error.orElse))
    )
