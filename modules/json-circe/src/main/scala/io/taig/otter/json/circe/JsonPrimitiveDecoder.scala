package io.taig.otter.json.circe

import cats.syntax.all.*
import io.taig.otter.*
import io.circe.Json
import io.circe.Decoder as CirceDecoder
import cats.data.Validated
import io.taig.otter.validation.Violations
import java.math.BigDecimal as JBigDecimal
import java.math.BigInteger as JBigInteger
import io.taig.otter.validation.Constraint
import io.taig.otter.validation.Violation
import io.circe.syntax.*

object JsonPrimitiveDecoder:
  def apply[A](schema: Primitive.Reader[A], json: Json): Validated[Violations[Json, Json], A] = schema match
    case Primitive.Reader.Optional(self) =>
      if json.isNull then none.valid[Violations[Json, Json]] else apply(self, json).map(_.some)
    case Primitive.Required.Reader.Root(tpe) =>
      apply(tpe, json).toValidated.leftMap: _ =>
        Violations.rootNec(Violation(Constraint.Type(typeOf(tpe)), typeOf(json).asJson))
    case Primitive.Required.Reader.Validate(schema, validation) =>
      apply(schema, json).andThen: a =>
        validation(a)
          .leftMap(_.map(_.bimap(JsonEncoder.apply, JsonEncoder.apply)))
          .leftMap(Violations.root)
    case Primitive.Reader.Validate(schema, validation) =>
      apply(schema, json).andThen: a =>
        validation(a)
          .leftMap(_.map(_.bimap(JsonEncoder.apply, JsonEncoder.apply)))
          .leftMap(Violations.root)
    case Primitive.Required(reader, _) => apply(reader, json)
    case Primitive.Optional(reader, _) => apply(reader, json)

  def apply[A](tpe: Type[A], json: Json): CirceDecoder.Result[A] = tpe match
    case Type.BigDecimal => json.as[JBigDecimal]
    case Type.BigInteger => json.as[JBigInteger]
    case Type.Boolean    => json.as[Boolean]
    case Type.Double     => json.as[Double]
    case Type.Float      => json.as[Float]
    case Type.Int        => json.as[Int]
    case Type.Long       => json.as[Long]
    case Type.String     => json.as[String]
