package io.taig.otter.sample.api.endpoints

import cats.data.Chain
import io.taig.otter.http.{Request, Response, Endpoint as OtterEndpoint}
import io.taig.otter.sample.api.{Role, headers, schemas}

final case class Endpoint[R <: Role, I, O](role: R) extends OtterEndpoint[Authentication[I], Either[Authentication.Error, O]] {
  override def request: Request[Authentication[I]] = ???

  override def request[T](f: Request[Authentication[I]] => Request[T]): OtterEndpoint[T, Either[Authentication.Error, O]] = ???

  override def response: Response[Either[Authentication.Error, O]] = ???

  override def response[T](f: Response[Either[Authentication.Error, O]] => Response[T]): OtterEndpoint[Authentication[I], T] = ???

  override def deprecated: Boolean = ???

  override def deprecated(f: Boolean => Boolean): OtterEndpoint[Authentication[I], Either[Authentication.Error, O]] = ???

  override def description: Option[String] = ???

  override def description(f: Option[String] => Option[String]): OtterEndpoint[Authentication[I], Either[Authentication.Error, O]] = ???

  override def hidden: Boolean = ???

  override def hidden(f: Boolean => Boolean): OtterEndpoint[Authentication[I], Either[Authentication.Error, O]] = ???

  override def operationId: Option[String] = ???

  override def operationId(f: Option[String] => Option[String]): OtterEndpoint[Authentication[I], Either[Authentication.Error, O]] = ???

  override def summary: Option[String] = ???

  override def summary(f: Option[String] => Option[String]): OtterEndpoint[Authentication[I], Either[Authentication.Error, O]] = ???

  override def tags: Chain[String] = ???

  override def tags(f: Chain[String] => Chain[String]): OtterEndpoint[Authentication[I], Either[Authentication.Error, O]] = ???
}

object Endpoint:
  def apply[R <: Role, I, O](role: R, request: Request[I], response: Response[O]): Endpoint[R, I, O] =
    OtterEndpoint(role, request, response)
      .request { request =>
        // TODO provide better syntax
        request
          .zip(headers.authorizationBearerUuid.optional.toHeaders)
          .imap { case (payload, session) =>
            Authentication(session, payload)
          }(authentication => (authentication.payload, authentication.session))
      }
      .response(_.modifyResults(schemas.authentication.error.orElse))
