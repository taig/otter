package io.taig.otter.sample.api.endpoints

import java.util.UUID
import scala.util.control.NoStackTrace

final case class Authentication[A](session: Option[UUID], payload: A)

object Authentication:
  enum Error extends NoStackTrace:
    case UserUnknown
    case Forbidden
