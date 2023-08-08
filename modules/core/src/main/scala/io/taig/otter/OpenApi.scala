package io.taig.otter

import scala.collection.immutable.VectorMap

sealed abstract class OpenApi:
  final def toValue: Option[OpenApi.Value] = this match
    case OpenApi.Null           => None
    case openapi: OpenApi.Value => Some(openapi)

  final def tpe: String = this match
    case _: OpenApi.Array  => "array"
    case _: OpenApi.Bool   => "boolean"
    case _: OpenApi.Number => "number"
    case _: OpenApi.Object => "object"
    case _: OpenApi.Text   => "string"
    case OpenApi.Null      => "null"

object OpenApi:
  sealed abstract class Value extends OpenApi

  sealed abstract class Primitive extends OpenApi.Value

  final case class Text(value: String) extends Primitive

  sealed abstract class Number extends Primitive
  final case class Integer(value: Int | Long | BigInt) extends Number
  final case class Decimal(value: Float | Double | BigDecimal) extends Number:
    def toBigDecimal: BigDecimal = value match
      case value: BigDecimal => value
      case value: Float      => BigDecimal(value)
      case value: Double     => BigDecimal(value)

  final case class Bool(value: Boolean) extends Primitive

  final case class Array(toVector: Vector[OpenApi]) extends OpenApi.Value

  final case class Object(toMap: VectorMap[String, OpenApi]) extends OpenApi.Value

  case object Null extends OpenApi
  type Null = Null.type
