package io.taig.otter

import cats.syntax.all.*

object TypeStringDecoder:
  def apply[A](tpe: Type[A], value: String): Option[A] =
    tpe match
      case Type.BigDecimal => ??? // json.as[JBigDecimal]
      case Type.BigInteger => ??? // json.as[JBigInteger]
      case Type.Boolean    => ??? // json.as[Boolean]
      case Type.Double     => ??? // json.as[Double]
      case Type.Float      => ??? // json.as[Float]
      case Type.Int        => ??? // json.as[Int]
      case Type.Long       => ??? // json.as[Long]
      case Type.String     => value.some
