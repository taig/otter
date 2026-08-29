package io.taig.otter.codec

import cats.data.Validated
import cats.syntax.all.*
import io.taig.otter.Constraint
import io.taig.otter.Tuple
import io.taig.otter.Violations
import io.taig.validation.Comparison
import io.taig.validation.Violation

final class TupleDecoder[F[-_, +_], T](decoder: Decoder[F, T], empty: T => Boolean)
    extends Decoder[[w, r] =>> Tuple[F, w, r], Vector[T]]:
  override def decode[R](schema: Tuple[F, Nothing, R], values: Vector[T]): Validated[Violations, R] =
    val expected = schema.schemas.length
    val actual = values.length

    if actual < expected then arity(Constraint.Collection.Minimum(Comparison(expected, exclusive = false)), actual)
    else if actual > expected then arity(Constraint.Collection.Maximum(Comparison(expected, exclusive = false)), actual)
    else positional(schema, values, index = 0)

  private def arity[R](constraint: Constraint.Collection, actual: Int): Validated[Violations, R] =
    Violations(Violation(constraint, actual, hint = none)).invalid

  /** The arity has already been checked by [[decode]], so `values(index)` is in range. */
  private def positional[R](schema: Tuple[F, Nothing, R], values: Vector[T], index: Int): Validated[Violations, R] =
    schema match
      case Tuple.Empty                  => ().valid
      case Tuple.Default(self, default) =>
        if values.forall(empty) then default.value.valid else positional(self, values, index)
      case Tuple.Modify(self, f, _) => positional(self, values, index).map(f)
      case Tuple.Optional(self)     =>
        if values.forall(empty) then Validated.Valid(None)
        else positional(self, values, index).map(Some.apply)
      case Tuple.Product(left, right) =>
        (positional(left, values, index), positional(right, values, index + left.schemas.length.toInt)).tupled
      case Tuple.Root(schema) => decoder.decode(schema.value, values(index)).leftMap(index /: _)
