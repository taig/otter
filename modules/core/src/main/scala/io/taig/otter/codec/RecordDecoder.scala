package io.taig.otter.codec

import cats.data.Chain
import cats.data.Validated
import cats.syntax.all.*
import io.taig.otter.Record
import io.taig.otter.Violations

@SuppressWarnings(Array("scalafix:DisableSyntax.asInstanceOf"))
final class RecordDecoder[F[-_, +_], T](decoder: Decoder.Remaining[F, Chain[(String, T)]])
    extends Decoder.Remaining[[w, r] =>> Record[F, w, r], Chain[(String, T)]]:
  override def decodeRemaining[R](
      schema: Record[F, Nothing, R],
      values: Chain[(String, T)]
  ): Validated[Violations, (Chain[(String, T)], R)] = (schema: @unchecked) match
    case Record.Empty                          => (values, ().asInstanceOf[R]).valid
    case schema: Record.Modify[F, ?, ?, ?, R]  => decodeRemaining(schema.self, values).map(_.map(schema.f))
    case schema: Record.Product[F, ?, ?, ?, ?] =>
      product(schema.left, schema.right, values).map(_.map(_.asInstanceOf[R]))
    case schema: Record.Root[F, ?, R] => decoder.decodeRemaining(schema.field.value, values)

  private def product[R1, R2](
      left: Record[F, Nothing, R1],
      right: Record[F, Nothing, R2],
      values: Chain[(String, T)]
  ): Validated[Violations, (Chain[(String, T)], (R1, R2))] =
    decodeRemaining(left, values) match
      case Validated.Valid((values, a)) =>
        decodeRemaining(right, values) match
          case Validated.Valid((values, b))  => (values, (a, b)).valid
          case result @ Validated.Invalid(_) => result
      case result @ Validated.Invalid(l) =>
        decodeRemaining(right, values) match
          case Validated.Valid(_)   => result
          case Validated.Invalid(r) => Validated.Invalid(l |+| r)
