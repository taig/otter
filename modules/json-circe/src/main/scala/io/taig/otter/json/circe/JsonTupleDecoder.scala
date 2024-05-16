package io.taig.otter.json.circe

import cats.syntax.all.*
import io.taig.otter as Base
import io.taig.otter.Plain.*
import io.circe.Json
import cats.data.Validated
import io.taig.otter.validation.Violations
import io.taig.otter.validation.Violation
import io.circe.syntax.*
import io.taig.otter.SchemaValidation
import cats.Id as Identity

object JsonTupleDecoder:
  def apply[A](schema: Tuple.Reader[A], values: Option[Vector[Json]]): Validated[Violations[Json, Json], A] =
    values match
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
          case Base.Tuple.Optional(_) | Base.Tuple.Reader.Optional(_) => none.valid[Violations[Json, Json]]
          case _ => Violations.rootNec(Violation.tpe("array", "null").map(_.asJson)).invalid

  def apply[A](
      schema: Tuple.Reader[A],
      values: Vector[Json]
  ): Validated[Violations[Json, Json], (Vector[Json], A)] = schema match
    case Base.Tuple.Empty                       => (values, ()).valid
    case Base.Tuple.Modify(self, validation, _) => modify(self, validation, values)
    case Base.Tuple.One(schema)                 =>
      // one(schema, values)
      ???
    case Base.Tuple.Optional(self)                  => optional(schema, values)
    case Base.Tuple.Product(left, right)            => product(left, right, values)
    case Base.Tuple.Reader.Empty                    => (values, ()).valid
    case Base.Tuple.Reader.Modify(self, validation) => modify(self, validation, values)
    case Base.Tuple.Reader.One(schema)              =>
      // one(schema, values)
      ???
    case Base.Tuple.Reader.Optional(self)       => optional(schema, values)
    case Base.Tuple.Reader.Product(left, right) => product(left, right, values)

  def modify[A, B, C, D](
      schema: Tuple.Reader[A],
      validation: SchemaValidation[A, B, C, D],
      values: Vector[Json]
  ): Validated[Violations[Json, Json], (Vector[Json], D)] = apply(schema, values).andThen:
    _.traverse: a =>
      validation(a)
        .leftMap(_.map(_.bimap(JsonEncoder.apply, JsonEncoder.apply)))
        .leftMap(Violations.root)

  def one[A](schema: Schema.Reader[A], values: Vector[Json]): Validated[Violations[Json, Json], (Vector[Json], A)] =
    values.headOption match
      case Some(head) => JsonDecoder(schema, head).tupleLeft(values.tail)
      case None       => Violations.rootNec(Violation.minItems(reference = 1, actual = 0).map(_.asJson)).invalid

  def optional[A](schema: Tuple.Reader[A], values: Vector[Json]): Validated[Violations[Json, Json], (Vector[Json], A)] =
    values.headOption match
      case Some(head) => JsonDecoder(schema, head).tupleLeft(values.tail)
      case None       =>
        // TODO incorrect error message, but unreachable case (?)
        Violations.rootNec(Violation.tpe("array", "null").map(_.asJson)).invalid

  def product[A, B](
      left: Tuple.Reader[A],
      right: Tuple.Reader[B],
      values: Vector[Json]
  ): Validated[Violations[Json, Json], (Vector[Json], (A, B))] = apply(left, values) match
    case Validated.Valid((values, a)) => apply(right, values).map(_.tupleLeft(a))
    case Validated.Invalid(violations) =>
      apply(right, values.drop(left.size)).fold(violations.combine, _ => violations).invalid
