package io.taig.otter.json.circe

import cats.syntax.all.*
import io.taig.otter.Type
import io.circe.Json
import io.circe.Decoder as CirceDecoder
import cats.data.Validated
import io.taig.otter.validation.Violations
import io.taig.otter as Base
import io.taig.otter.Plain.*
import java.math.BigDecimal as JBigDecimal
import java.math.BigInteger as JBigInteger
import io.taig.otter.validation.Violation
import io.circe.syntax.*

object JsonPrimitiveDecoder:
  def apply[A](schema: Primitive.Reader[A], json: Json): Validated[Violations[Json, Json], A] = schema match
    case Base.Primitive.Optional(self)                            => optional(self, json)
    case Base.Primitive.Invariant(self, validation, _)            => functor(self, validation, json)
    case Base.Primitive.Required.Invariant(self, validation, _)   => functor(self, validation, json)
    case Base.Primitive.Required.Reader.Functor(self, validation) => functor(self, validation, json)
    case Base.Primitive.Required.Root(_, tpe) =>
      root(tpe, json).toValidated.leftMap: _ =>
        Violations.rootNec(Violation.tpe(typeOf(tpe), typeOf(json)).map(_.asJson))
    case Base.Primitive.Reader.Functor(self, validation) => functor(self, validation, json)
    case Base.Primitive.Reader.Optional(self)            => optional(self, json)

  def functor[A, V1, V2, B](
      self: Primitive.Reader[A],
      validation: Validation[A, V1, V2, B],
      json: Json
  ): Validated[Violations[Json, Json], B] = apply(self, json).andThen:
    validation
      .apply(_)
      .leftMap(_.map(_.bimap(JsonEncoder.apply, JsonEncoder.apply)))
      .leftMap(Violations.root)

  def optional[A](self: Primitive.Reader[A], json: Json): Validated[Violations[Json, Json], Option[A]] =
    if json.isNull then none.valid else apply(self, json).map(_.some)

  def root[A](tpe: Type[A], json: Json): CirceDecoder.Result[A] = tpe match
    case Type.BigDecimal => json.as[JBigDecimal]
    case Type.BigInteger => json.as[JBigInteger]
    case Type.Boolean    => json.as[Boolean]
    case Type.Double     => json.as[Double]
    case Type.Float      => json.as[Float]
    case Type.Int        => json.as[Int]
    case Type.Long       => json.as[Long]
    case Type.String     => json.as[String]
