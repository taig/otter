package io.taig.otter.json.circe

import cats.syntax.all.*
import io.taig.otter.*
import io.circe.Json
import io.circe.Decoder as CirceDecoder
import cats.data.Validated
import io.taig.otter.validation.Violations
import java.math.BigDecimal as JBigDecimal
import java.math.BigInteger as JBigInteger
import io.taig.otter.validation.Violation
import io.circe.syntax.*

object JsonPrimitiveDecoder:
  def apply[A](schema: Primitive.Reader[A], json: Json): Validated[Violations[Json, Json], A] = schema match
    case Primitive.Modify(self, validation, _)              => modify(self, validation, json)
    case Primitive.Optional(self)                           => optional(self, json)
    case Primitive.Reader.Modify(self, validation)          => modify(self, validation, json)
    case Primitive.Reader.Optional(self)                    => optional(self, json)
    case Primitive.Required.Modify(self, validation, _)     => modify(self, validation, json)
    case Primitive.Required.Reader.Modify(self, validation) => modify(self, validation, json)
    case Primitive.Root(tpe) =>
      apply(tpe, json).toValidated.leftMap: _ =>
        Violations.rootNec(Violation.tpe(typeOf(tpe), typeOf(json)).map(_.asJson))

  def modify[A, B, C, D, E](
      self: Primitive.Reader[A],
      validation: SchemaValidation[A, B, C, D],
      json: Json
  ): Validated[Violations[Json, Json], D] = apply(self, json).andThen: a =>
    validation(a)
      .leftMap(_.map(_.bimap(JsonEncoder.apply, JsonEncoder.apply)))
      .leftMap(Violations.root)

  def optional[A](self: Primitive.Reader[A], json: Json): Validated[Violations[Json, Json], Option[A]] =
    if json.isNull then none.valid[Violations[Json, Json]] else apply(self, json).map(_.some)

  def apply[A](tpe: Type[A], json: Json): CirceDecoder.Result[A] = tpe match
    case Type.BigDecimal => json.as[JBigDecimal]
    case Type.BigInteger => json.as[JBigInteger]
    case Type.Boolean    => json.as[Boolean]
    case Type.Double     => json.as[Double]
    case Type.Float      => json.as[Float]
    case Type.Int        => json.as[Int]
    case Type.Long       => json.as[Long]
    case Type.String     => json.as[String]
