package io.taig.otter.codec

import io.taig.otter.Tuple
import cats.data.Validated
import cats.syntax.all.*
import io.taig.otter.Violations
import io.taig.otter.Violation
import io.taig.otter.Constraint

final class TupleDecoder[S[_], T](decoder: Decoder[S, T]) extends Decoder[Tuple[S, *], Vector[T]]:
  override def apply[A](schema: Tuple[S, A], values: Vector[T]): Validated[Violations, A] =
    val reference = schema.schemas.size.toInt
    val actual = values.size

    Validated.cond(
      test = actual <= reference,
      (),
      Violations.rootNec(Violation(Constraint.Collection.Maximum(reference), actual, hint = none))
    ) *> Validated.cond(
      test = actual >= reference,
      (),
      Violations.rootNec(Violation(Constraint.Collection.Minimum(reference), actual, hint = none))
    ) *> apply(schema, values, index = 0)

  def apply[A](schema: Tuple[S, A], values: Vector[T], index: Int): Validated[Violations, A] =
    schema match
      case Tuple.Empty(_)           => ().valid
      case Tuple.Modify(self, f, _) => apply(schema = self, values, index).map(f)
      case Tuple.Root(schema, _) =>
        values.headOption
          .toValid(Violations.rootNec(Violation.required))
          .andThen(decoder(schema = schema.value, _))
          .leftMap(index /: _)
      case Tuple.Zip(left, right, _) =>
        val size = left.schemas.size.toInt
        values
          .splitAt(size)
          .bimap(apply(schema = left, _, index), apply(schema = right, _, index + size))
          .tupled
