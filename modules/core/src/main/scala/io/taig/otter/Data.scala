package io.taig.otter

import cats.Eq
import cats.data.Chain
import cats.syntax.all.*
import java.lang.Float as JFloat
import java.lang.Double as JDouble

import java.lang.String as JString
import scala.{Boolean as SBoolean, Product as SProduct}

sealed abstract class Data extends SProduct with Serializable:
  final def toValue: Option[Data.Value] = this match
    case data: Data.Value => Some(data)
    case Data.Null        => None

  final def toObject: Option[Data.Object] = this match
    case data: Data.Object => Some(data)
    case _                 => None

  final def toArray: Option[Data.Array] = this match
    case data: Data.Array => Some(data)
    case _                => None

  final def name: String = this match
    case _: Data.Array   => "array"
    case _: Data.Boolean => "boolean"
    case _: Data.Number  => "number"
    case _: Data.Object  => "object"
    case _: Data.String  => "string"
    case Data.Null       => "null"

object Data:
  sealed abstract class Value extends Data

  final case class Object(values: Chain[(JString, Data)]) extends Data.Value:
    def ++(obj: Data.Object): Data.Object = Object(values ++ obj.values)

  object Object:
    val Empty: Data.Object = Object(Chain.empty)
    def one(key: JString, value: Data): Data.Object = Object(Chain.one((key, value)))
    def of(kv: (JString, Data)*): Data.Object = Object(Chain.fromSeq(kv))

  final case class Array(values: Vector[Data]) extends Data.Value:
    def length: Long = values.length
    def ++(data: Data.Array): Data.Array = Array(values ++ data.values)

  object Array:
    val Empty: Data.Array = Array(Vector.empty)
    def fill(n: Long)(value: => Data): Data.Array = Array(Vector.fill(n.toInt)(value))

  sealed abstract class Primitive extends Value

  final case class String(value: JString) extends Data.Primitive

  final case class Boolean(value: SBoolean) extends Data.Primitive

  final case class Number(value: Int | Long | Float | Double | BigDecimal | BigInt) extends Data.Primitive:
    def asInt: Option[Int] = asLong.flatMap: value =>
      val asInt = value.toInt
      Option.when(value == asInt)(asInt)

    def asLong: Option[Long] = value match
      case value: Long => Some(value)
      case value: Int  => Some(value.toLong)
      case value: BigDecimal =>
        Option.when(bigDecimalIsValidLong(value))(value.longValue)
      case value: Float =>
        val asBigDecimal = BigDecimal(value.toDouble)
        Option.when(bigDecimalIsValidLong(asBigDecimal))(asBigDecimal.longValue)
      case value: Double =>
        val asBigDecimal = BigDecimal(value)
        Option.when(bigDecimalIsValidLong(asBigDecimal))(asBigDecimal.longValue)
      case value: BigInt =>
        val asBigDecimal = BigDecimal(value)
        Option.when(bigDecimalIsValidLong(asBigDecimal))(asBigDecimal.longValue)

    def asBigInt: Option[BigInt] = toBigDecimal.toBigIntExact

    def toFloat: Float = value match
      case value: Float      => value
      case value: Int        => value.toFloat
      case value: Long       => value.toFloat
      case value: Double     => value.toFloat
      case value: BigDecimal => value.toFloat
      case value: BigInt     => value.toFloat

    def toDouble: Double = value match
      case value: Double     => value
      case value: Float      => value.toDouble
      case value: Int        => value.toDouble
      case value: Long       => value.toDouble
      case value: BigDecimal => value.toDouble
      case value: BigInt     => value.toDouble

    def toBigDecimal: BigDecimal = value match
      case value: BigDecimal => value
      case value: Int        => BigDecimal(value)
      case value: Long       => BigDecimal(value)
      case value: Float      => BigDecimal(JFloat.toString(value))
      case value: Double     => BigDecimal(JDouble.toString(value))
      case value: BigInt     => BigDecimal(value)

  case object Null extends Data

  given Eq[Data] = Eq.fromUniversalEquals

private val bigDecimalMinLong: BigDecimal = BigDecimal(Long.MinValue)
private val bigDecimalMaxLong: BigDecimal = BigDecimal(Long.MaxValue)

private def bigDecimalIsWhole(value: BigDecimal): Boolean =
  value.signum == 0 || value.scale <= 0 || value.bigDecimal.stripTrailingZeros.scale <= 0

private def bigDecimalIsValidLong(value: BigDecimal): Boolean =
  bigDecimalIsWhole(value) && value.compareTo(bigDecimalMinLong) >= 0 && value.compareTo(bigDecimalMaxLong) <= 0
