package io.taig.otter

import scala.collection.immutable.VectorMap

sealed abstract class OpenApi:
  final def asValue: Option[OpenApi.Value] = this match
    case openapi: OpenApi.Value => Some(openapi)
    case OpenApi.Null           => None

  final def asBool: Option[OpenApi.Bool] = this match
    case openapi: OpenApi.Bool => Some(openapi)
    case _                     => None

  final def asText: Option[OpenApi.Text] = this match
    case openapi: OpenApi.Text => Some(openapi)
    case _                     => None

  final def asNumber: Option[OpenApi.Number] = this match
    case openapi: OpenApi.Number => Some(openapi)
    case _                       => None

  final def asInteger: Option[OpenApi.Integer] = this match
    case openapi: OpenApi.Integer => Some(openapi)
    case _                        => None

  final def asDecimal: Option[OpenApi.Decimal] = this match
    case openapi: OpenApi.Decimal => Some(openapi)
    case _                        => None

  final def asArray: Option[OpenApi.Array] = this match
    case openapi: OpenApi.Array => Some(openapi)
    case _                      => None

  final def asObject: Option[OpenApi.Object] = this match
    case openapi: OpenApi.Object => Some(openapi)
    case _                       => None

  final def asNull: Option[OpenApi.Null] = this match
    case OpenApi.Null => Some(OpenApi.Null)
    case _            => None

  final def as[A: Decoder]: Option[A] = Decoder[A].decode(this)

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
  final case class Decimal(value: Float | Double | BigDecimal) extends Number

  final case class Bool(value: Boolean) extends Primitive

  final case class Array(toVector: Vector[OpenApi]) extends OpenApi.Value

  final case class Object(toMap: VectorMap[String, OpenApi]) extends OpenApi.Value

  case object Null extends OpenApi
  type Null = Null.type
