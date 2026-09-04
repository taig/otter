package io.taig.otter.codec

import cats.data.Validated
import cats.syntax.all.*
import io.taig.otter.Record
import io.taig.otter.Violations

final class RecordDecoder[F[-_, +_], T](decoder: Decoder.Remaining[F, Fields[T]])
    extends Decoder.Remaining[[w, r] =>> Record[F, w, r], Fields[T]]:
  override def decodeRemaining[R](
      schema: Record[F, Nothing, R],
      values: Fields[T]
  ): Validated[Violations, (Fields[T], R)] = schema match
    case Record.Empty                => (values, ()).valid
    case Record.Modify(self, f, _)   => decodeRemaining(self, values).map(_.map(f))
    case Record.Product(left, right) => product(left, right, values)
    case Record.Root(field)          => decoder.decodeRemaining(field.value, values)

  private def product[R1, R2](
      left: Record[F, Nothing, R1],
      right: Record[F, Nothing, R2],
      values: Fields[T]
  ): Validated[Violations, (Fields[T], (R1, R2))] =
    decodeRemaining(left, values) match
      case Validated.Valid((values, a)) =>
        decodeRemaining(right, values) match
          case Validated.Valid((values, b))  => (values, (a, b)).valid
          case result @ Validated.Invalid(_) => result
      case result @ Validated.Invalid(l) =>
        decodeRemaining(right, values) match
          case Validated.Valid(_)   => result
          case Validated.Invalid(r) => Validated.Invalid(l |+| r)
