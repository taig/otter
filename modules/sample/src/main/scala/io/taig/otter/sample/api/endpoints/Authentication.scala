package io.taig.otter.sample.api.endpoints

import io.taig.otter.sample.api.Roles

import scala.util.control.NoStackTrace

final case class Authentication[R, A, B](roles: Roles[R], self: Option[A], payload: B)

object Authentication:
  enum Error extends NoStackTrace:
    case UserUnknown
    case Forbidden
