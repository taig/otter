package io.taig.openapi.sample

import java.util.UUID
import scala.util.control.NoStackTrace

final case class Authorization[A](token: UUID, payload: A)

object Authorization:
  enum Error extends NoStackTrace:
    case Unauthorized
    case Forbidden
