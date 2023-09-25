package io.taig.otter.sample.api.endpoints

import scala.util.control.NoStackTrace

final case class Authentication[A, B](self: Option[A], payload: B)

object Authentication:
  enum Error extends NoStackTrace:
    case UserUnknown
    case Forbidden
