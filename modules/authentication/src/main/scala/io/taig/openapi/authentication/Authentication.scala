package io.taig.openapi.authentication

import scala.util.control.NoStackTrace

final case class Authentication[F[_], A, B](user: F[A], payload: B)

object Authentication:
  enum Error extends NoStackTrace:
    case UserUnknown
    case AccessForbidden
