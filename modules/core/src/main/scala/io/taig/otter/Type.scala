package io.taig.otter

import cats.Show

import java.lang.String as JString

sealed abstract class Type extends Product with Serializable

object Type:
  case object Boolean extends Type

  enum Number extends Type:
    case BigDecimal
    case BigInteger
    case Double
    case Float
    case Int
    case Long

  enum String extends Type:
    case Value
    case Parser(name: JString)

  given [A <: Type]: Show[A] =
    case Boolean             => "boolean"
    case Number.BigDecimal   => "bigDecimal"
    case Number.BigInteger   => "bigInteger"
    case Number.Double       => "double"
    case Number.Float        => "float"
    case Number.Int          => "int"
    case Number.Long         => "long"
    case String.Value        => "string"
    case String.Parser(name) => name
