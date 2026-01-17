package io.taig.otter.codec

import cats.data.Chain
import cats.data.Validated
import cats.data.Validated.Invalid
import cats.data.Validated.Valid
import cats.syntax.all.*
import io.taig.otter.Record
import io.taig.otter.Violations

final class RecordDecoder[F[_], A](decoder: Decoder.Remaining[F, Chain[(String, A)]])
    extends Decoder.Remaining[Record.Read[F, *], Chain[(String, A)]]:
  override def decodeRemaining[B](
      schema: Record.Read[F, B],
      values: Chain[(String, A)]
  ): Validated[Violations, (Chain[(String, A)], B)] = schema match
    case Record.Empty                     => (values, ()).valid
    case Record.Modify(self, f, _)        => decodeRemaining(self, values).map(_.map(f))
    case Record.Read.Modify(self, f)      => decodeRemaining(self, values).map(_.map(f))
    case Record.Read.Product(left, right) => decodeProduct(left, right, values)
    case Record.Root(field)               => decoder.decodeRemaining(field.value, values)
    case Record.Product(left, right)      => decodeProduct(left, right, values)

  def decodeProduct[B, C](
      left: Record.Read[F, B],
      right: Record.Read[F, C],
      values: Chain[(String, A)]
  ): Validated[Violations, (Chain[(String, A)], (B, C))] =
    decodeRemaining(left, values) match
      case Validated.Valid((values, a)) =>
        decodeRemaining(right, values) match
          case Validated.Valid((values, b))  => (values, (a, b)).valid
          case result @ Validated.Invalid(_) => result
      case result @ Validated.Invalid(left) =>
        decodeRemaining(right, values) match
          case Validated.Valid(_)       => result
          case Validated.Invalid(right) => Validated.Invalid(left |+| right)
