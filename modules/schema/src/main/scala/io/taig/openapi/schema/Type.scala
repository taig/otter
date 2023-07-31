package io.taig.openapi.schema

import cats.syntax.all.*

enum Type[A]:
  case BigDecimal extends Type[BigDecimal]
  case BigInt extends Type[BigInt]
  case Boolean extends Type[Boolean]
  case Double extends Type[Double]
  case Float extends Type[Float]
  case Int extends Type[Int]
  case Long extends Type[Long]
  case String extends Type[String]
