package io.taig.openapi.sample

import cats.effect.IO
import cats.syntax.all.*

import java.util.UUID
import scala.reflect.{ClassTag, Typeable}

final class Authentication(admin: UUID, member: UUID):
  def apply[I, O](endpoint: AuthorizedEndpoint[I, O])(f: I => IO[O]): AuthorizedEndpoint.Implementation[I, O] =
    endpoint.implementedBy { authorization =>
      if authorization.token === admin || authorization.token === member && endpoint.role === Role.Member then
        f(authorization.payload)
      else if authorization.token === member then IO.pure(Authorization.Error.Forbidden)
      else IO.pure(Authorization.Error.Unauthorized)
    }

object Authentication:
  val Admin = UUID.fromString("00000000-0000-0000-0000-000000000000")
  val Member = UUID.fromString("00000000-0000-0000-0000-000000000001")

  def default: Authentication = new Authentication(Admin, Member)
