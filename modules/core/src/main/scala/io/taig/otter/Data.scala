package io.taig.otter

import cats.Eq
import cats.data.Chain
import cats.syntax.all.*
import java.math.BigDecimal as JBigDecimal
import java.math.BigInteger as JBigInteger
import java.lang.Math.toIntExact

import java.lang.String as JString
import scala.{Boolean as SBoolean, Product as SProduct}
import cats.Applicative
import cats.Monad
import cats.Traverse
import scala.reflect.TypeTest
import cats.Id

sealed abstract class Data extends SProduct with Serializable:
  final def asValue: Option[Data.Value] = this match
    case data: Data.Value => Some(data)
    case _                => None

  final def asObject: Option[Data.Object[?]] = this match
    case data: Data.Object[?] => Some(data)
    case _                    => None

  final def asArray: Option[Data.Array[?]] = this match
    case data: Data.Array[?] => Some(data)
    case _                   => None

  final def asPrimitive: Option[Data.Primitive] = this match
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
  sealed abstract class Value extends Data

  final case class Object[+A <: Data](values: Vector[(JString, A)]) extends Data.Value:
    def ++[B <: Data](obj: Data.Object[B]): Data.Object[A | B] = Object(values ++ obj.values)
    def +[B <: Data](kv: (JString, B)): Data.Object[A | B] = Object(values :+ kv)
    final def map[B <: Data](f: A => B): Data.Object[B] = Object(values.map(_.map(f)))
    final def filterKeys(f: JString => SBoolean): Data.Object[A] = Object(values.filter { case (key, _) => f(key) })

  object Object:
    val Empty: Data.Object[Nothing] = Object(Vector.empty)
    def one[A <: Data](key: JString, value: A): Data.Object[A] = Object(Vector((key, value)))
    def of[A <: Data](kv: (JString, A)*): Data.Object[A] = Object(kv.toVector)
    def fromOption[A <: Data](kv: Option[(JString, A)]): Data.Object[A] = Object(kv.toVector)

  final case class Array[+A <: Data](values: Vector[A]) extends Data.Value:
    def length: Long = values.length
    def ++[B <: Data](data: Data.Array[B]): Data.Array[A | B] = Array(values ++ data.values)

  object Array:
    val Empty: Data.Array[Nothing] = Array(Vector.empty)
    def one[A <: Data](data: A): Data.Array[A] = Data.Array(Vector(data))
    def of[A <: Data](data: A*): Data.Array[A] = Data.Array(data.toVector)
    def fill[A <: Data](n: Int)(value: => A): Data.Array[A] = Array(Vector.fill(n)(value))

  sealed abstract class Primitive extends Value:
    def asString: Option[Data.String] = this match
      case data: Data.String => data.some
      case _                 => none

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

  type Optional[A <: Data] = A | Data.Null.type

  extension [A <: Data.Value: TypeTest[Data.Optional[A], *]](self: Data.Optional[A])
    def getOrElse[B >: A](b: => B): B = self match
      case Data.Null => b
      case a: A      => a

  trait Ops[F[+a <: Data] <: Data.Optional[a]]:
    extension [A <: Data](self: F[Data.Array[A]]) def sequence(n: Int): Data.Array[F[A]]
    extension [A <: Data](self: F[Data.Object[A]]) def sequence(fields: Vector[JString]): Data.Object[F[A]]

  object Ops:
    given Ops[Id] = new Ops[Id]:
      extension [A <: Data](self: Data.Array[A]) override def sequence(n: Int): Data.Array[A] = self
      extension [A <: Data](self: Data.Object[A]) override def sequence(fields: Vector[JString]): Data.Object[A] = self

    given Ops[Data.Optional] = new Ops[Data.Optional]:
      extension [A <: Data](self: Data.Optional[Data.Object[A]])
        override def sequence(fields: Vector[JString]): Data.Object[Data.Optional[A]] = self match
          case Data.Null            => Data.Object(fields.tupleRight(Data.Null))
          case data: Data.Object[A] => data

      extension [A <: Data](self: Data.Optional[Data.Array[A]])
        override def sequence(n: Int): Data.Array[Data.Optional[A]] = self match
          case Data.Null           => Data.Array.fill(n)(Data.Null)
          case data: Data.Array[A] => data

  given Eq[Data] = Eq.fromUniversalEquals
