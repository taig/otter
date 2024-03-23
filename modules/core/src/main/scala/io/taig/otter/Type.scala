package io.taig.otter

import cats.syntax.all.*

import java.lang.String as JString
import scala.{BigDecimal as SBigDecimal, BigInt as SBigInt}

enum Type[A]:
  case BigDecimal extends Type[SBigDecimal]
  case BigInt extends Type[SBigInt]
  case Boolean extends Type[Boolean]
  case Double extends Type[Double]
  case Float extends Type[Float]
  case Int extends Type[Int]
  case Long extends Type[Long]
  case String extends Type[JString]
