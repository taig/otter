package io.taig.otter.codec

import cats.data.Validated
import cats.syntax.all.*
import io.taig.otter.Constraint
import io.taig.otter.Tuple
import io.taig.otter.Violations
import io.taig.validation.Comparison
import io.taig.validation.Violation

@SuppressWarnings(Array("scalafix:DisableSyntax.asInstanceOf"))
final class TupleDecoder[F[- _, + _], T](decoder: Decoder[F, T], empty: T => Boolean)
    extends Decoder[[w, r] =>> Tuple[F, w, r], Vector[T]]:
  override def decode[R](schema: Tuple[F, Nothing, R], values: Vector[T]): Validated[Violations, R] =
    val expected = schema.schemas.length
    val actual = values.length

    if actual < expected then arity(Constraint.Collection.Minimum(Comparison(expected, exclusive = false)), actual)
    else if actual > expected then arity(Constraint.Collection.Maximum(Comparison(expected, exclusive = false)), actual)
    else unsafeDecode(schema, values, index = 0)

  private def arity[R](constraint: Constraint.Collection, actual: Int): Validated[Violations, R] =
    Violations(Violation(constraint, actual, hint = none)).invalid

  private def unsafeDecode[R](schema: Tuple[F, Nothing, R], values: Vector[T], index: Int): Validated[Violations, R] =
    (schema: @unchecked) match
      case Tuple.Empty                    => ().asInstanceOf[R].valid
      case schema: Tuple.Default[F, ?, R] =>
        if values.forall(empty) then schema.value.value.valid else unsafeDecode(schema.self, values, index)
      case schema: Tuple.Modify[F, ?, ?, ?, R] => unsafeDecode(schema.self, values, index).map(schema.f)
      case schema: Tuple.Optional[F, ?, ?]     =>
        if values.forall(empty) then none.asInstanceOf[R].valid
        else unsafeDecode(schema.self, values, index).map(_.some.asInstanceOf[R])
      case schema: Tuple.Product[F, ?, ?, ?, ?] =>
        (
          unsafeDecode(schema.left, values, index),
          unsafeDecode(schema.right, values, index = index + schema.left.schemas.length.toInt)
        ).tupled.map(_.asInstanceOf[R])
      case schema: Tuple.Root[F, ?, R] => decoder.decode(schema.schema.value, values(index)).leftMap(index /: _)
