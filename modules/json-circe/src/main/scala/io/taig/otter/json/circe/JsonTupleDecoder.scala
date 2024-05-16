package io.taig.otter.json.circe

import cats.syntax.all.*
import io.taig.otter.Tuple
import io.circe.Json
import cats.data.Validated
import io.taig.otter.validation.Violations
import io.taig.otter.validation.Violation
import io.circe.syntax.*
import io.taig.otter.Schema
import io.taig.otter.SchemaValidation
import cats.Id as Identity

object JsonTupleDecoder:
  def apply[A](
      schema: Tuple.Reader[Identity, ?, A],
      values: Option[Vector[Json]]
  ): Validated[Violations[Json, Json], A] = values match
    case Some(values) =>
      val expected = schema.size
      val actual = values.length
      if expected > actual then
        Violations.rootNec(Violation.minItems(reference = expected, actual).map(_.asJson)).invalid
      else if expected < actual then
        Violations.rootNec(Violation.maxItems(reference = expected, actual).map(_.asJson)).invalid
      else apply(schema, values).map { case (_, a) => a }
    case None =>
      schema match
        case Tuple.Optional(_) | Tuple.Reader.Optional(_) => none.valid[Violations[Json, Json]]
        case _ => Violations.rootNec(Violation.tpe("array", "null").map(_.asJson)).invalid

  def apply[A](
      schema: Tuple.Reader[Identity, ?, A],
      values: Vector[Json]
  ): Validated[Violations[Json, Json], (Vector[Json], A)] = schema match
    case Tuple.Empty                           => (values, ()).valid
    case Tuple.Modify(self, validation, _)     => modify(self, validation, values)
    case Tuple.One(schema)                     => one(schema, values)
    case Tuple.Optional(self)                  => optional(schema, values)
    case Tuple.Product(left, right)            => product(left, right, values)
    case Tuple.Reader.Empty                    => (values, ()).valid
    case Tuple.Reader.Modify(self, validation) => modify(self, validation, values)
    case Tuple.Reader.One(schema)              => one(schema, values)
    case Tuple.Reader.Optional(self)           => optional(schema, values)
    case Tuple.Reader.Product(left, right)     => product(left, right, values)

  def modify[A, B, C, D](
      schema: Tuple.Reader[Identity, ?, A],
      validation: SchemaValidation[A, B, C, D],
      values: Vector[Json]
  ): Validated[Violations[Json, Json], (Vector[Json], D)] = apply(schema, values).andThen:
    _.traverse: a =>
      validation(a)
        .leftMap(_.map(_.bimap(JsonEncoder.apply, JsonEncoder.apply)))
        .leftMap(Violations.root)

  def one[A](
      schema: Schema.Reader[Identity, ?, A],
      values: Vector[Json]
  ): Validated[Violations[Json, Json], (Vector[Json], A)] = values.headOption match
    case Some(head) => JsonDecoder(schema, head).tupleLeft(values.tail)
    case None       => Violations.rootNec(Violation.minItems(reference = 1, actual = 0).map(_.asJson)).invalid

  def optional[A](
      schema: Tuple.Reader[Identity, ?, A],
      values: Vector[Json]
  ): Validated[Violations[Json, Json], (Vector[Json], A)] = values.headOption match
    case Some(head) => JsonDecoder(schema, head).tupleLeft(values.tail)
    case None       =>
      // TODO incorrect error message, but unreachable case (?)
      Violations.rootNec(Violation.tpe("array", "null").map(_.asJson)).invalid

  def product[A, B](
      left: Tuple.Reader[Identity, ?, A],
      right: Tuple.Reader[Identity, ?, B],
      values: Vector[Json]
  ): Validated[Violations[Json, Json], (Vector[Json], (A, B))] = apply(left, values) match
    case Validated.Valid((values, a)) => apply(right, values).map(_.tupleLeft(a))
    case Validated.Invalid(violations) =>
      apply(right, values.drop(left.size)).fold(violations.combine, _ => violations).invalid
