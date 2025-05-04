package io.taig.otter.http

import cats.derived.*
import cats.Eq
import io.taig.otter.Violations

final case class Response[+S[_], +T[_], A](
    result: Result[S, A],
    errors: Result[T, Response.Error],
    failure: Result[T, Option[String]]
):
  def modifyResult[U[a] >: S[a], B](f: Result[S, A] => Result[U, B]): Response[U, T, B] = copy(result = f(result))

  def imap[B](f: A => B)(g: B => A): Response[S, T, B] = copy(result = result.imap(f)(g))

object Response:
  enum Error derives Eq:
    case ContentNegotiationFailed // (violations: Violations)
    // case MediaTypesUnsupported(violations: Violations)
    case ValidationViolations(violations: Violations)
