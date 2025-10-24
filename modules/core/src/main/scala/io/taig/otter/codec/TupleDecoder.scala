package io.taig.otter.codec

import cats.data.Validated
import cats.syntax.all.*
import io.taig.otter.Constraint
import io.taig.otter.Tuple
import io.taig.otter.Violations
import io.taig.validation.Comparison
import io.taig.validation.Violation

final class TupleDecoder[-S[_], T](decoder: Decoder[S, T], empty: T => Boolean) extends Decoder[Tuple[S, *], Vector[T]]:
  override def decode[A](schema: Tuple[S, A], values: Vector[T]): Validated[Violations, A] =
    val reference = schema.schemas.length
    val actual = values.length

    if actual < reference
    then
      Violations(
        Violation(
          constraint = Constraint.Collection.Minimum(
            comparison = Comparison(reference, exclusive = false)
          ),
          actual,
          hint = none
        )
      ).invalid
    else if actual > reference
    then
      Violations(
        Violation(
          constraint = Constraint.Collection.Maximum(
            comparison = Comparison(reference, exclusive = false)
          ),
          actual,
          hint = none
        )
      ).invalid
    else unsafeDecode(schema, values, index = 0)

  def unsafeDecode[A](schema: Tuple[S, A], values: Vector[T], index: Int): Validated[Violations, A] = schema match
    case Tuple.Default(self, default) =>
      if values.forall(empty)
      then default.valid
      else unsafeDecode(schema = self, values, index)
    case Tuple.Empty              => ().valid
    case Tuple.Modify(self, f, _) => unsafeDecode(schema = self, values, index).map(f)
    case Tuple.Optional(self)     =>
      if values.forall(empty)
      then None.valid
      else unsafeDecode(schema = self, values, index).map(_.some)
    case Tuple.Root(schema) =>
      decoder.decode(schema = schema.value, values.head).leftMap(index /: _)
    case Tuple.Zip(left, right) =>
      (
        unsafeDecode(schema = left, values, index),
        unsafeDecode(schema = right, values, index = index + left.schemas.length.toInt)
      ).tupled
