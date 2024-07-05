package io.taig.otter

import cats.syntax.all.*

import java.lang.String as JString
import java.math.BigDecimal as JBigDecimal
import java.math.BigInteger as JBigInteger

enum Type[A]:
  case BigDecimal extends Type[JBigDecimal]
  case BigInteger extends Type[JBigInteger]
  case Boolean extends Type[Boolean]
  case Double extends Type[Double]
  case Float extends Type[Float]
  case Int extends Type[Int]
  case Long extends Type[Long]
  case String extends Type[JString]

  final override def toString: String = this match
    case Type.BigDecimal | Type.BigInteger | Type.Double | Type.Float | Type.Int | Type.Long => "number"
    case Type.Boolean                                                                        => "boolean"
    case Type.String                                                                         => "string"
