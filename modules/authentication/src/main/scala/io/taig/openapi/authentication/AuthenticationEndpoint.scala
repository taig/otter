package io.taig.openapi.authentication

import cats.Id
import io.taig.openapi.http.{Endpoint, Header, Input, Output}
import io.taig.openapi.http.syntax.*

type UnauthenticatedEndpoint[I, O] = Endpoint[I, O]

object UnauthenticatedEndpoint:
  type Implementation[F[_], I, O] = Endpoint.Implementation[F, I, O]

  def apply[I, O](input: Input[I], output: Output[O]): UnauthenticatedEndpoint[I, O] = Endpoint(input, output)

  inline def union[I, E <: Matchable, O <: Matchable](
      input: Input[I],
      errors: Output.Results[E],
      result: Output.Results[O]
  ): UnauthenticatedEndpoint[I, E | O] = UnauthenticatedEndpoint(input, output(errors or result))

  def either[I, E, O](
      input: Input[I],
      errors: Output.Results[E],
      result: Output.Results[O]
  ): UnauthenticatedEndpoint[I, Either[E, O]] = UnauthenticatedEndpoint(input, output(errors orElse result))

type AuthenticatedEndpoint[F[_], A, I, O] = Endpoint[Authentication[F, A, I], Either[Authentication.Error, O]]

object AuthenticatedEndpoint:
  type Implementation[F[_], G[_], A, I, O] =
    Endpoint.Implementation[F, Authentication[G, A, I], Either[Authentication.Error, O]]

  def apply[F[_], A, I, O](
      authentication: Header[F[A]]
  )(endpoint: UnauthenticatedEndpoint[I, O]): AuthenticatedEndpoint[F, A, I, O] = endpoint
    .modifyInput { input =>
      (input :* authentication).imap { case (payload, user) => Authentication[F, A, I](user, payload) } {
        authentication => (authentication.payload, authentication.user)
      }
    }
    .modifyOutput(_.modifyResults(schemas.authentication.errors.orElse))
