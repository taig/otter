package io.taig.otter

import cats.Eq
import cats.data.Chain
import cats.syntax.all.*
import java.math.BigDecimal as JBigDecimal
import java.math.BigInteger as JBigInteger
import java.lang.Float as JFloat
import java.lang.Double as JDouble

import java.lang.String as JString
import scala.{Boolean as SBoolean, Product as SProduct}

sealed abstract class Data extends SProduct with Serializable:
  final def isNull: Boolean = this match
    case Data.Null => true
    case _         => false

  final def name: String = this match
    case _: Data.Array   => "array"
    case _: Data.Boolean => "boolean"
    case _: Data.Number  => "number"
    case _: Data.Object  => "object"
    case _: Data.String  => "string"
    case Data.Null       => "null"

object Data:
  sealed trait Value extends Data

  final case class Object(values: Chain[(JString, Data)]) extends Value:
    def ++(obj: Data.Object): Data.Object = Object(values ++ obj.values)

  object Object:
    val Empty: Data.Object = Object(Chain.empty)
    def one(key: JString, value: Data): Data.Object = Object(Chain.one((key, value)))
    def of(kv: (JString, Data)*): Data.Object = Object(Chain.fromSeq(kv))

  final case class Array(values: Chain[Data]) extends Value:
    def length: Long = values.length
    def ++(data: Data.Array): Data.Array = Array(values ++ data.values)

  object Array:
    val Empty: Data.Array = Array(Chain.empty)
    def fill(n: Long)(value: => Data): Data.Array = Array(Chain.fromSeq(Seq.fill(n.toInt)(value)))

  sealed abstract class Primitive extends Value

  final case class String(value: JString) extends Data.Primitive

  final case class Boolean(value: SBoolean) extends Data.Primitive

  final case class Number(value: Int | Long | Float | Double | JBigDecimal | JBigInteger) extends Data.Primitive:
    def toBigDecimal: JBigDecimal = value match
      case value: JBigDecimal => value
      case value: Int         => JBigDecimal(value)
      case value: Long        => JBigDecimal(value)
      case value: Float       => JBigDecimal(JFloat.toString(value))
      case value: Double      => JBigDecimal(JDouble.toString(value))
      case value: JBigInteger => JBigDecimal(value)

    def toBigInteger: Option[JBigInteger] = value match
      case value: JBigInteger => Some(value)
      case value =>
        try Some(toBigDecimal.toBigIntegerExact())
        catch { case _: ArithmeticException => None }

    def toDouble: Double = value match
      case value: Double      => value
      case value: Float       => value.toDouble
      case value: Int         => value.toDouble
      case value: Long        => value.toDouble
      case value: JBigDecimal => value.doubleValue()
      case value: JBigInteger => value.doubleValue()

    def toFloat: Float = value match
      case value: Float       => value
      case value: Int         => value.toFloat
      case value: Long        => value.toFloat
      case value: Double      => value.toFloat
      case value: JBigDecimal => value.floatValue()
      case value: JBigInteger => value.floatValue()

    def toInt: Option[Int] = toLong.flatMap: value =>
      val int = value.toInt
      Option.when(value == int)(int)

    def toLong: Option[Long] = value match
      case value: Long => Some(value)
      case value: Int  => Some(value.toLong)
      case value: JBigDecimal =>
        Option.when(bigDecimalIsValidLong(value))(value.longValue)
      case value: Float =>
        val bigDecimal = JBigDecimal(value.toDouble)
        Option.when(bigDecimalIsValidLong(bigDecimal))(bigDecimal.longValue)
      case value: Double =>
        val bigDecimal = JBigDecimal(value)
        Option.when(bigDecimalIsValidLong(bigDecimal))(bigDecimal.longValue)
      case value: JBigInteger =>
        val bigDecimal = JBigDecimal(value)
        Option.when(bigDecimalIsValidLong(bigDecimal))(bigDecimal.longValue)

  case object Null extends Data

  given Eq[Data] = Eq.fromUniversalEquals

private val bigDecimalMinLong: JBigDecimal = JBigDecimal(Long.MinValue)
private val bigDecimalMaxLong: JBigDecimal = JBigDecimal(Long.MaxValue)

private def bigDecimalIsWhole(value: JBigDecimal): Boolean =
  value.signum == 0 || value.scale <= 0 || value.stripTrailingZeros.scale <= 0

private def bigDecimalIsValidLong(value: JBigDecimal): Boolean =
  bigDecimalIsWhole(value) && value.compareTo(bigDecimalMinLong) >= 0 && value.compareTo(bigDecimalMaxLong) <= 0
