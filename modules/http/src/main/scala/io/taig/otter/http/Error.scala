package io.taig.otter.http

import io.taig.otter.Violations
import cats.Show

final case class Error[A](tpe: A, violations: Option[Violations])

object Error:
  def apply[A](tpe: A, violations: Violations): Error[A] = Error(tpe, Some(violations))

  def apply[A](tpe: A): Error[A] = Error(tpe, None)

  given [A: Show]: Show[Error[A]] = Printers(_)
