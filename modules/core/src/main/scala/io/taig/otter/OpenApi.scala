package io.taig.otter

import cats.data.{Chain, Validated}
import io.taig.otter.validation.Violation

import scala.collection.immutable.VectorMap
import java.lang.String as JString
import scala.Boolean as SBoolean

sealed abstract class OpenApi:
  final def asValue: Option[OpenApi.Value] = this match
    case openapi: OpenApi.Value => Some(openapi)
    case OpenApi.Null           => None

  final def asBool: Option[OpenApi.Boolean] = this match
    case openapi: OpenApi.Boolean => Some(openapi)
    case _                        => None

  final def asText: Option[OpenApi.String] = this match
    case openapi: OpenApi.String => Some(openapi)
    case _                       => None

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

  final def as[A: Decoder]: Validated[Violation, A] = Decoder[A].decode(this)

  final def tpe: JString = this match
    case _: OpenApi.Array   => "array"
    case _: OpenApi.Boolean => "boolean"
    case _: OpenApi.Number  => "number"
    case _: OpenApi.Object  => "object"
    case _: OpenApi.String  => "string"
    case OpenApi.Null       => "null"

object OpenApi:
  sealed abstract class Value extends OpenApi

  sealed abstract class Primitive extends OpenApi.Value

  final case class String(value: JString) extends Primitive

  sealed abstract class Number extends Primitive

  final case class Integer(value: Int | Long | BigInt) extends Number
  final case class Decimal(value: Float | Double | BigDecimal) extends Number

  final case class Boolean(value: SBoolean) extends Primitive

  final case class Array(toChain: Chain[OpenApi]) extends OpenApi.Value:
    def ++(array: OpenApi.Array): OpenApi.Array = Array(toChain ++ array.toChain)
    def :+(openapi: OpenApi): OpenApi.Array = Array(toChain :+ openapi)
    def +:(openapi: OpenApi): OpenApi.Array = Array(openapi +: toChain)

  object Array:
    val Empty: OpenApi.Array = Array(Chain.empty)

  final case class Object(toMap: VectorMap[JString, OpenApi]) extends OpenApi.Value:
    def ++(obj: OpenApi.Object): OpenApi.Object = Object(toMap ++ obj.toMap)

  object Object:
    val Empty: OpenApi.Object = Object(VectorMap.empty)

  case object Null extends OpenApi
  type Null = Null.type

  def arr(values: OpenApi*): OpenApi.Array = OpenApi.Array(Chain.fromSeq(values))
  def obj(values: (JString, OpenApi)*): OpenApi.Object = OpenApi.Object(VectorMap.from(values))
