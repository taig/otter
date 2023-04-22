package io.taig.openapi.sample

import cats.effect.IO
import cats.syntax.all.*
import io.taig.openapi.*
import io.taig.openapi.http.*
import io.taig.openapi.dsl.*

type UnauthorizedEndpoint[I, O] = Endpoint[I, O]

object UnauthorizedEndpoint:
  type Implementation[I, O] = Endpoint.Implementation[IO, I, O]

  def apply[I, O](input: Input[I], output: Output[O]): UnauthorizedEndpoint[I, O] = Endpoint(input, output)

  def apply[I, E, O](
      input: Input[I],
      errors: Output.Results[E],
      success: Output.Result[O]
  ): UnauthorizedEndpoint[I, Either[E, O]] = Endpoint(input, output(errors, success))

final case class AuthorizedEndpoint[I, O](
    role: Role,
    endpoint: UnauthorizedEndpoint[Authorization[I], Authorization.Error | O]
):
  def implementedBy(
      f: Authorization[I] => IO[Authorization.Error | O]
  ): AuthorizedEndpoint.Implementation[I, O] = Endpoint.Implementation(endpoint, f)

object AuthorizedEndpoint:
  type Implementation[I, O] = Endpoint.Implementation[IO, Authorization[I], Authorization.Error | O]
