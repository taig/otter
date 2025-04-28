package io.taig.otter.http

import cats.derived.*
import cats.Eq
import io.taig.otter.Violations

final case class Response[+S[_], A](
    result: Result[S, A],
    errors: Result[S, Route.Error],
    failure: Result[S, Option[String]]
):
  def modifyResult[T[a] >: S[a], B](f: Result[S, A] => Result[T, B]): Response[T, B] = copy(result = f(result))

  def imap[B](f: A => B)(g: B => A): Response[S, B] = copy(result = result.imap(f)(g))

object Response:
  enum Error derives Eq:
    case ContentNegotiationFailed(violations: Violations)
    case MediaTypesUnsupported(violations: Violations)
    case ValidationViolations(violations: Violations)
