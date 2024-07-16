package io.taig.otter

import cats.Eq
import cats.data.Chain
import cats.syntax.all.*
import java.lang.Float as JFloat
import java.lang.Double as JDouble
import java.math.BigDecimal as JBigDecimal
import java.math.BigInteger as JBigInteger

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

  final def toPrimitive: Option[Data.Primitive] = this match
    case data: Data.Primitive => Some(data)
    case _                    => None

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

  final case class Number(value: Int | Long | Float | Double | JBigDecimal | JBigInteger) extends Data.Primitive:
    def toBigDecimal: Option[JBigDecimal] = ???

    def toBigInteger: Option[JBigInteger] = ???

    def toDouble: Option[Double] = ???

    def toFloat: Option[Float] = ???

    def toInt: Option[Int] = ???

    def toLong: Option[Long] = ???

  case object Null extends Data

  given Eq[Data] = Eq.fromUniversalEquals
