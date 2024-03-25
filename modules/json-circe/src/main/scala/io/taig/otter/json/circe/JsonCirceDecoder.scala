package io.taig.otter.json.circe

import cats.syntax.all.*
import io.taig.otter.*
import io.circe.Json
import io.circe.Decoder as CirceDecoder
import cats.data.Validated
import io.taig.otter.validation.Violations

object JsonCirceDecoder extends Decoder[Json]:
  def apply[A](schema: Primitive[?, A], json: Json): Validated[Violations, A] = schema match
    case Primitive.Required.Root(_, tpe)            => apply(tpe, json).toValidated.leftMap(_ => ???)
    case Primitive.Required.Modify(primitive, f, _) => apply(primitive, json).map(f)
    case Primitive.Required.Update(primitive, _)    => apply(primitive, json)
    case Primitive.Optional.Modify(primitive, f, _) => apply(primitive, json).map(f)
    case Primitive.Optional.Root(primitive)         => apply(primitive, json).map(_.some)
    case Primitive.Optional.Update(primitive, _)    => apply(primitive, json)

  def apply[A](tpe: Type[A], json: Json): CirceDecoder.Result[A] = tpe match
    case Type.BigDecimal => json.as[BigDecimal]
    case Type.BigInt     => json.as[BigInt]
    case Type.Boolean    => json.as[Boolean]
    case Type.Double     => json.as[Double]
    case Type.Float      => json.as[Float]
    case Type.Int        => json.as[Int]
    case Type.Long       => json.as[Long]
    case Type.String     => json.as[String]

  def apply[A](schema: Product[?, A], json: Json): Validated[Violations, A] = ???
