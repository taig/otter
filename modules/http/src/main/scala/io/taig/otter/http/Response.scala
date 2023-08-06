package io.taig.otter.http

import io.taig.otter.schema.Violations
import io.taig.otter.validation.Validation

final case class Response[+A](results: Results[A], violations: Result[Violations])

object Response:
  sealed abstract class Body[A]:
    self =>
    type Self[a] <: Body[a] { type Self[a] = self.Self[a] }
    def ivalidate[B](validation: Validation[A, B])(g: B => A): Self[B]
    final def validate(validation: Validation[A, Unit]): Self[A] = ivalidate(validation.tap)(identity)
    final def imap[B](f: A => B)(g: B => A): Self[B] = ivalidate(Validation.lift(f))(g)
