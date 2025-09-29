package io.taig.otter.codec

import cats.data.Validated
import cats.syntax.all.*
import io.taig.otter.Tuple
import io.taig.otter.Violations
import io.taig.otter.validation.Constraint
import io.taig.otter.validation.Violation

final class TupleDecoder[S[_], T](decoder: Decoder[S, T]) extends Decoder[Tuple[S, *], Seq[T]]:
  override def decode[A](schema: Tuple[S, A], values: Seq[T]): Validated[Violations, A] =
    val reference = schema.value.schemas.size.toInt
    val actual = values.size

    Validated.cond(
      test = actual <= reference,
      (),
      Violations.rootNec(Violation(Constraint.Collection.Maximum(reference), actual, hint = none))
    ) *> Validated.cond(
      test = actual >= reference,
      (),
      Violations.rootNec(Violation(Constraint.Collection.Minimum(reference), actual, hint = none))
    ) *> decode(schema.value, values, index = 0)

  def decode[A](schema: Tuple.Value[S, A], values: Seq[T], index: Int): Validated[Violations, A] = schema match
    case Tuple.Value.Empty              => ().valid
    case Tuple.Value.Modify(self, f, _) => decode(schema = self, values, index).map(f)
    case Tuple.Value.Root(schema)       =>
      values.headOption
        .toValid(Violations.rootNec(Violation.required))
        .andThen(decoder.decode(schema = schema.value, _))
        .leftMap(index /: _)
    case Tuple.Value.Zip(left, right) =>
      val size = left.schemas.size.toInt
      values
        .splitAt(size)
        .bimap(decode(schema = left, _, index), decode(schema = right, _, index + size))
        .tupled
