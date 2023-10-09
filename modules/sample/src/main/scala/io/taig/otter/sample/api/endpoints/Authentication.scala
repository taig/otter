package io.taig.otter.sample.api.endpoints

import io.taig.otter.sample.data.Session

import scala.util.control.NoStackTrace

final case class Authentication[A](session: Option[Session], payload: A)

object Authentication:
  enum Error extends NoStackTrace:
    case UserUnknown
    case Forbidden
