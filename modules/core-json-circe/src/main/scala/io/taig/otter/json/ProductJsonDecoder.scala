package io.taig.otter.json

import cats.syntax.all.*
import io.taig.otter.*
import io.circe.Json
import cats.data.Validated
import io.taig.otter.validation.Violations
import io.taig.otter.validation.Violation
import io.circe.syntax.*
import io.taig.otter.Decoder

object ProductJsonDecoder:
  def apply[A](schema: Product.Reader.Via[Json, A], values: Option[Vector[Json]]): Decoder.Result[Json, A] =
    // TODO disallow values with additional items
    withRemainders(schema, values).map(_._2)

  // TODO add index to errors
  def withRemainders[A](
      schema: Product.Reader.Via[Json, A],
      values: Option[Vector[Json]]
  ): Decoder.Result[Json, (Option[Vector[Json]], A)] = schema match
    case Product.Combine(_, left, right)            => combine(left, right, values)
    case Product.Empty(_)                           => (values, ()).valid
    case Product.One(_, schema)                     => one(schema, values)
    case Product.Optional(self)                     => optional(self, values)
    case Product.Reader.Combine(_, left, right)     => combine(left, right, values)
    case Product.Reader.One(_, schema)              => one(schema, values)
    case Product.Reader.Optional(self)              => optional(self, values)
    case Product.Reader.Transform(self, validation) => transform(self, validation, values)
    case Product.Transform(self, validation, _)     => transform(self, validation, values)

  def combine[A, B](
      left: Product.Reader.Via[Json, A],
      right: Product.Reader.Via[Json, B],
      values: Option[Vector[Json]]
  ): Decoder.Result[Json, (Option[Vector[Json]], (A, B))] = withRemainders(left, values) match
    case Validated.Valid((remainders, a)) =>
      withRemainders(right, remainders).map(_.tupleLeft(a))
    case Validated.Invalid(violations) =>
      ProductJsonDecoder(right, values.map(_.drop(left.schemas.size.toInt)))
        .fold(violations.combine(_), _ => violations)
        .invalid

  def one[A](
      schema: Schema.Reader.Via[Json, A],
      values: Option[Vector[Json]]
  ): Decoder.Result[Json, (Option[Vector[Json]], A)] = values
    .toValid(Violations.rootNec(Violation(Constraint.Type(name = "array"), actual = "null").map(_.asJson)))
    .andThen: values =>
      values.headOption match
        case Some(head) => JsonDecoder(schema, head).tupleLeft(values.tail.some)
        case None =>
          Violations.rootNec(Violation(Constraint.Collection.MinItems(reference = 1), actual = 0.asJson)).invalid

  def optional[A](
      self: Product.Reader.Via[Json, A],
      values: Option[Vector[Json]]
  ): Decoder.Result[Json, (Option[Vector[Json]], Option[A])] =
    values.fold((none, none).valid)(_ => withRemainders(self, values).map(_.map(_.some)))

  def transform[A, B](
      self: Product.Reader.Via[Json, A],
      f: A => B,
      values: Option[Vector[Json]]
  ): Decoder.Result[Json, (Option[Vector[Json]], B)] = withRemainders(self, values).map(_.map(f))
