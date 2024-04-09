package io.taig.otter.json.circe

import cats.syntax.all.*
import io.taig.otter.*
import io.circe.Json
import io.circe.Decoder as CirceDecoder
import cats.data.Validated
import io.taig.otter.validation.Violations
import java.math.BigDecimal as JBigDecimal
import java.math.BigInteger as JBigInteger
import cats.data.Chain

object JsonDecoder extends Decoder[Json]:
  def decode[A](schema: Primitive[?, A], json: Json): Validated[Violations[Json], A] = schema match
    case Primitive.Required.Root(_, tpe)         => decode(tpe, json).toValidated.leftMap(_ => ???) // TODO adjust error
    case Primitive.Required.Modify(schema, f, _) => decode(schema, json).map(f)
    case Primitive.Optional.Modify(schema, f, _) => decode(schema, json).map(f)
    case Primitive.Optional.Root(schema)         => decode(schema, json).map(_.some)
    case Primitive.Optional.Validate(schema, constraint, validation, g) =>
      decode(schema, json).andThen(a =>
        validation
          .apply(a)
          .leftMap(violations => Violations.root(violations.map(_.map(c => JsonEncoder.apply(constraint, c)))))
      )

  def decode[A](tpe: Type[A], json: Json): CirceDecoder.Result[A] = tpe match
    case Type.BigDecimal => json.as[JBigDecimal]
    case Type.BigInteger => json.as[JBigInteger]
    case Type.Boolean    => json.as[Boolean]
    case Type.Double     => json.as[Double]
    case Type.Float      => json.as[Float]
    case Type.Int        => json.as[Int]
    case Type.Long       => json.as[Long]
    case Type.String     => json.as[String]

  def decode[A](schema: Tuple[?, A], json: Json): Validated[Violations[Json], A] =
    if json.isNull then JsonTupleDecoder.decode(schema, none)
    else
      json.asArray match
        case Some(values) => JsonTupleDecoder.decode(schema, Chain.fromSeq(values).some)
        case None         => ??? // error
