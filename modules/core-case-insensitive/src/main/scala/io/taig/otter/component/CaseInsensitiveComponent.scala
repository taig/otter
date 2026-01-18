package io.taig.otter.component

import cats.Invariant
import cats.syntax.all.*
import io.taig.otter.operation.PrimitiveOperation
import io.taig.validation.Constraint
import io.taig.validation.Validation
import org.typelevel.ci.CIString

trait CaseInsensitiveComponent[F[_]: Invariant](using F: PrimitiveOperation.Text[F]):
  final def cistring(validation: Validation[Constraint.Primitive.Text, CIString]): F[CIString] =
    F.string(validation = validation.contramap(CIString.apply)).imap(CIString.apply)(_.toString)

  final val cistring: F[CIString] = cistring(validation = Validation.valid)
