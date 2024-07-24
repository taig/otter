package io.taig.otter

import cats.Eq
import cats.data.Chain
import cats.syntax.all.*
import java.math.BigDecimal as JBigDecimal
import java.math.BigInteger as JBigInteger
import java.lang.Math.toIntExact

import java.lang.String as JString
import scala.{Boolean as SBoolean, Product as SProduct}

sealed abstract class Data[+A <: Data[?]] extends SProduct with Serializable:
  final def toObject[A1 >: A <: Data[?]]: Option[Data.Object[A1]] = this match
    case data: Data.Object[?] => Some(data)
    case _                    => None

  final def toArray[A1 >: A <: Data[?]]: Option[Data.Array[A1]] = this match
    case data: Data.Array[?] => Some(data)
    case _                   => None

  final def toPrimitive: Option[Data.Primitive] = this match
    case data: Data.Primitive => Some(data)
    case _                    => None

  final def name: String = this match
    case _: Data.Array[?]  => "array"
    case _: Data.Boolean   => "boolean"
    case _: Data.Number    => "number"
    case _: Data.Object[?] => "object"
    case _: Data.String    => "string"
    case Data.Null         => "null"

object Data:
  extension [A <: Data[?]](self: Data[A])
    final def toValue: Option[Data.Value[A]] = self match
      case data: Data.Value[A] => Some(data)
      case Data.Null           => None

  sealed abstract class Value[+A <: Data[?]] extends Data[A]

  final case class Object[+A <: Data[?]](values: Chain[(JString, A)]) extends Data.Value[A]:
    def ++[A1 >: A <: Data[?]](obj: Data.Object[A1]): Data.Object[A1] = Object(values ++ obj.values)

  object Object:
    val Empty: Data.Object[Nothing] = Object(Chain.empty)
    def one[A <: Data[?]](key: JString, value: A): Data.Object[A] = Object(Chain.one((key, value)))
    def of[A <: Data[?]](kv: (JString, A)*): Data.Object[A] = Object(Chain.fromSeq(kv))
    def fromOption[A <: Data[?]](kv: Option[(JString, A)]): Data.Object[A] = Object(Chain.fromOption(kv))
    def fromSeq[A <: Data[?]](kvs: Seq[(JString, A)]): Data.Object[A] = Object(Chain.fromSeq(kvs))

  final case class Array[+A <: Data[?]](values: Vector[A]) extends Data.Value[A]:
    def length: Long = values.length
    def ++[A1 >: A <: Data[?]](data: Data.Array[A1]): Data.Array[A1] = Array(values ++ data.values)

  object Array:
    val Empty: Data.Array[Nothing] = Array(Vector.empty)
    def one[A <: Data[?]](data: A): Data.Array[A] = Data.Array(Vector(data))
    def fill[A <: Data[?]](n: Long)(value: => A): Data.Array[A] = Array(Vector.fill(n.toInt)(value))

  sealed abstract class Primitive extends Value[Nothing]

  final case class String(value: JString) extends Data.Primitive

  final case class Boolean(value: SBoolean) extends Data.Primitive

  final case class Number(value: Int | Long | Float | Double | JBigDecimal | JBigInteger) extends Data.Primitive:
    def toBigDecimal: Option[JBigDecimal] = value match
      case value: Int         => JBigDecimal.valueOf(value).some
      case value: Long        => JBigDecimal.valueOf(value).some
      case value: Float       => JBigDecimal.valueOf(value.toDouble).some
      case value: Double      => JBigDecimal.valueOf(value).some
      case value: JBigDecimal => value.some
      case value: JBigInteger => new JBigDecimal(value).some

    def toBigInteger: Option[JBigInteger] = value match
      case value: Int         => JBigInteger.valueOf(value).some
      case value: Long        => JBigInteger.valueOf(value).some
      case value: Float       => attempt(JBigDecimal.valueOf(value).toBigIntegerExact())
      case value: Double      => attempt(JBigDecimal.valueOf(value).toBigIntegerExact())
      case value: JBigDecimal => attempt(value.toBigIntegerExact())
      case value: JBigInteger => value.some

    def toDouble: Option[Double] = value match
      case value: Int         => value.toDouble.some
      case value: Long        => value.toDouble.some
      case value: Float       => value.toDouble.some
      case value: Double      => value.some
      case value: JBigDecimal => attempt(value, _.doubleValue())
      case value: JBigInteger => attempt(value, _.doubleValue())

    def toFloat: Option[Float] = value match
      case value: Int         => value.toFloat.some
      case value: Long        => value.toFloat.some
      case value: Float       => value.some
      case value: Double      => attempt(value, _.toFloat)
      case value: JBigDecimal => attempt(value, _.floatValue())
      case value: JBigInteger => attempt(value, _.floatValue())

    def toInt: Option[Int] = value match
      case value: Int    => value.some
      case value: Long   => attempt(toIntExact(value))
      case value: Float  => Option.when(value >= Int.MinValue && value <= Int.MaxValue && value % 1 == 0)(value.toInt)
      case value: Double => Option.when(value >= Int.MinValue && value <= Int.MaxValue && value % 1 == 0)(value.toInt)
      case value: JBigDecimal => attempt(value.intValueExact())
      case value: JBigInteger => attempt(value.intValueExact())

    def toLong: Option[Long] = value match
      case value: Int   => value.toLong.some
      case value: Long  => value.some
      case value: Float => Option.when(value >= Long.MinValue && value <= Long.MaxValue && value % 1 == 0)(value.toLong)
      case value: Double =>
        Option.when(value >= Long.MinValue && value <= Long.MaxValue && value % 1 == 0)(value.toLong)
      case value: JBigDecimal => attempt(value.longValueExact())
      case value: JBigInteger => attempt(value.longValueExact())

    private def attempt[A](f: => A): Option[A] = try f.some
    catch { case _: ArithmeticException => none }

    private def attempt[A, B](value: A, convert: A => B): Option[B] =
      val target = convert(value)
      Option.when(value == target)(target)

  case object Null extends Data

  type Optional[A <: Data[?]] = A | Data.Null.type

  given [A <: Data[?]]: Eq[Data[A]] = Eq.fromUniversalEquals
