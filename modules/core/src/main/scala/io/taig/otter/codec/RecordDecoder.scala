package io.taig.otter.codec

import cats.data.Chain
import cats.data.Validated
import cats.data.Validated.Invalid
import cats.data.Validated.Valid
import cats.syntax.all.*
import io.taig.otter.Record
import io.taig.otter.Violations

final class RecordDecoder[F[_], T](decoder: Decoder.Remaining[F, Chain[(String, T)]])
    extends Decoder.Remaining[Record.Read[F, *], Chain[(String, T)]]:
  override def decodeRemaining[A](
      schema: Record.Read[F, A],
      values: Chain[(String, T)]
  ): Validated[Violations, (Chain[(String, T)], A)] = schema match
    case Record.Empty                     => (values, ()).valid
    case Record.Modify(self, f, _)        => decodeRemaining(self, values).map(_.map(f))
    case Record.Read.Modify(self, f)      => decodeRemaining(self, values).map(_.map(f))
    case Record.Read.Product(left, right) => decodeProduct(left, right, values)
    case Record.Root(field)               => decoder.decodeRemaining(field.value, values)
    case Record.Product(left, right)      => decodeProduct(left, right, values)

  def decodeProduct[A, B](
      left: Record.Read[F, A],
      right: Record.Read[F, B],
      values: Chain[(String, T)]
  ): Validated[Violations, (Chain[(String, T)], (A, B))] =
    decodeRemaining(left, values) match
      case Validated.Valid((values, a)) =>
        decodeRemaining(right, values) match
          case Validated.Valid((values, b))  => (values, (a, b)).valid
          case result @ Validated.Invalid(_) => result
      case result @ Validated.Invalid(left) =>
        decodeRemaining(right, values) match
          case Validated.Valid(_)       => result
          case Validated.Invalid(right) => Validated.Invalid(left |+| right)
