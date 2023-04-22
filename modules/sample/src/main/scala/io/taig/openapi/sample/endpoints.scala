package io.taig.openapi.sample

import cats.syntax.all.*
import io.taig.openapi.*
import io.taig.openapi.dsl.*
import io.taig.openapi.http.{Endpoint, Input, Method, Output, Path, Url}
import io.taig.openapi.schema

import scala.util.control.NoStackTrace

object endpoints:
  val errors: Output.Results[Authorization.Error] = (
    result(code.unauthorized, singleton(Authorization.Error.Unauthorized)) +
      result(code.forbidden, singleton(Authorization.Error.Forbidden))
  ).gimap

  inline def authorized[I, O <: Matchable](role: Role)(endpoint: UnauthorizedEndpoint[I, O]): AuthorizedEndpoint[I, O] =
    AuthorizedEndpoint(
      role,
      endpoint
        .modifyInput { input =>
          (input :* headers.authorization).imap { case (payload, token) =>
            Authorization(token, payload)
          }(authorization => (authorization.payload, authorization.token))
        }
        .modifyOutput(_.modifyResults((o: Output.Results[O]) => o.or(errors)))
    )

  def endpoint[I, E, O](
      input: Input[I],
      errors: Output.Results[E],
      success: Output.Result[O]
  ): UnauthorizedEndpoint[I, Either[E, O]] = UnauthorizedEndpoint(input, errors, success)

  def endpoint[I, O](input: Input[I], success: Output.Result[O]): UnauthorizedEndpoint[I, O] =
    UnauthorizedEndpoint(input, output(success))

  object pets:
    val Root: Path[Unit] = __ / "pets"

    val delete: AuthorizedEndpoint[Unit, Unit] = authorized(Role.Admin) {
      endpoint(input(method.delete, Root), result(code.ok))
    }

    val get: UnauthorizedEndpoint[Option[Animal], Pets] = endpoint(
      input(method.get, Root ? query("type", schemas.animal.optional)),
      result(code.ok, schemas.pets)
    )

    enum Post extends NoStackTrace:
      case MaxPetsExceeded(limit: Int)

    val post: AuthorizedEndpoint[Pet, Either[Post, Pets]] = authorized(Role.Member) {
      endpoint(
        input(method.post, Root, body.json(schemas.pet)),
        result(
          code.badRequest,
          error("max-pets-exceeded", field("limit", int).gimap[Post.MaxPetsExceeded])
        ).gimap[Post].toResults,
        result(code.ok, schemas.pets)
      )
    }
