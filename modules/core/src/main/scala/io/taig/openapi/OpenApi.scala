package io.taig.openapi

import cats.data.Chain
import cats.syntax.all.*

import java.lang.String as SString
import scala.collection.immutable.SeqMap
import scala.{
  BigDecimal as SBigDecimal,
  BigInt as SBigInt,
  Boolean as SBoolean,
  Double as SDouble,
  Float as SFloat,
  Int as SInt,
  Long as SLong
}

/** AST that represents the OpenAPI data types
  *
  * @see
  *   https://swagger.io/docs/specification/data-models/data-types/
  */
sealed abstract class OpenApi extends Product with Serializable:
  final def asArray: Option[OpenApi.Array[OpenApi]] = this match
    case value: OpenApi.Array[?] => Some(value)
    case _                       => None

  final def asObject: Option[OpenApi.Object] = this match
    case value: OpenApi.Object => Some(value)
    case _                     => None

  final def asPrimitive: Option[OpenApi.Primitive] = this match
    case value: OpenApi.Primitive => Some(value)
    case _                        => None

  final def asNumber: Option[OpenApi.Number] = this match
    case value: OpenApi.Number => Some(value)
    case OpenApi.String(value) =>
      try Some(OpenApi.BigDecimal(SBigDecimal(value)))
      catch case _: NumberFormatException => None
    case _ => None

  final def asDecimal: Option[OpenApi.Decimal] = this match
    case value: OpenApi.Decimal => Some(value)
    case OpenApi.String(value) =>
      try Some(OpenApi.BigDecimal(SBigDecimal(value)))
      catch case _: NumberFormatException => None
    case _ => None

  final def asInteger: Option[OpenApi.Integer] = this match
    case value: OpenApi.Integer => Some(value)
    case OpenApi.String(value) =>
      try Some(OpenApi.BigInt(SBigInt(value)))
      catch case _: NumberFormatException => None
    case _ => None

  final def asValue: Option[OpenApi.Value] = this match
    case OpenApi.Null         => None
    case value: OpenApi.Value => Some(value)

  final def isNull: Boolean = this match
    case OpenApi.Null => true
    case _            => false

  final def find(history: History): Option[OpenApi] = history.initLast match
    case None                                      => Some(this)
    case Some((history, History.Step.Field(name))) => asObject.flatMap(_.get(name)).flatMap(_.find(history))
    case Some((history, History.Step.Index(i)))    => asArray.flatMap(_.get(i)).flatMap(_.find(history))

  final def merge(openapi: OpenApi): OpenApi = (this, openapi) match
    case (left: OpenApi.Array[?], right: OpenApi.Array[?]) => left ++ right
    case (left: OpenApi.Object, right: OpenApi.Object)     => left.deepMerge(right)
    case (left, OpenApi.Null)                              => left
    case (_, right)                                        => right

object OpenApi:
  case object Null extends OpenApi

  sealed abstract class Value extends OpenApi

  sealed abstract class Primitive extends Value:
    final def toBigDecimal: Option[OpenApi.BigDecimal] = this match
      case openapi: BigDecimal => openapi.some
      case BigInt(value)       => BigDecimal(SBigDecimal(value)).some
      case Double(value) =>
        try BigDecimal(SBigDecimal.valueOf(value)).some
        catch case _: NumberFormatException => none
      case Float(value) =>
        try BigDecimal(SBigDecimal.valueOf(value.toDouble)).some
        catch case _: NumberFormatException => none
      case Int(value)  => BigDecimal(SBigDecimal(value)).some
      case Long(value) => BigDecimal(SBigDecimal(value)).some
      case String(value) =>
        try BigDecimal(SBigDecimal(value)).some
        catch case _: NumberFormatException => none
      case _: Boolean => none

    final def toBigInt: Option[OpenApi.BigInt] = this match
      case openapi: BigInt   => openapi.some
      case BigDecimal(value) => value.toBigIntExact.map(BigInt.apply)
      case Double(value) =>
        try SBigDecimal.valueOf(value).toBigIntExact.map(BigInt.apply)
        catch case _: NumberFormatException => none
      case Float(value) =>
        try SBigDecimal.valueOf(value.toDouble).toBigIntExact.map(BigInt.apply)
        catch case _: NumberFormatException => none
      case Int(value)  => BigInt(SBigInt(value)).some
      case Long(value) => BigInt(SBigInt(value)).some
      case String(value) =>
        try BigInt(SBigInt(value)).some
        catch case _: NumberFormatException => none
      case _: Boolean => none

    final def toDouble: Option[OpenApi.Double] = this match
      case openapi: Double   => openapi.some
      case BigDecimal(value) => Double(value.doubleValue).some
      case BigInt(value)     => Double(value.doubleValue).some
      case Float(value)      => Double(value.toDouble).some
      case Int(value)        => Double(value.toDouble).some
      case Long(value)       => Double(value.toDouble).some
      case String(value)     => value.toDoubleOption.map(Double.apply)
      case _: Boolean        => none

    final def toFloat: Option[OpenApi.Float] = this match
      case openapi: Float    => openapi.some
      case BigDecimal(value) => Float(value.floatValue).some
      case BigInt(value)     => Float(value.floatValue).some
      case Double(value)     => Float(value.toFloat).some
      case Int(value)        => Float(value.toFloat).some
      case Long(value)       => Float(value.toFloat).some
      case String(value)     => value.toFloatOption.map(Float.apply)
      case _: Boolean        => none

    final def toInt: Option[OpenApi.Int] = this match
      case openapi: Int => openapi.some
      case BigDecimal(value) =>
        try Int(value.toIntExact).some
        catch case _: ArithmeticException => none
      case BigInt(value) => Option.when(value.isValidInt)(Int(value.intValue))
      case Double(value) => Some(value.toInt).filter(_ == value).map(Int.apply)
      case Float(value)  => Some(value.toInt).filter(_ == value).map(Int.apply)
      case Long(value)   => Some(value.toInt).filter(_ == value).map(Int.apply)
      case String(value) => value.toIntOption.map(Int.apply)
      case _: Boolean    => none

    final def toLong: Option[OpenApi.Long] = this match
      case openapi: Long => openapi.some
      case BigDecimal(value) =>
        try Long(value.toLongExact).some
        catch case _: ArithmeticException => none
      case BigInt(value) => Option.when(value.isValidLong)(Long(value.intValue))
      case Double(value) => Some(value.toLong).filter(_ == value).map(Long.apply)
      case Float(value)  => Some(value.toLong).filter(_ == value).map(Long.apply)
      case Int(value)    => Long(value.toLong).some
      case String(value) => value.toLongOption.map(Long.apply)
      case _: Boolean    => none

    final def toBoolean: Option[OpenApi.Boolean] = this match
      case openapi: Boolean => openapi.some
      case String("true")   => Boolean.True.some
      case String("false")  => Boolean.False.some
      case _                => none

    final def render: SString = this match
      case OpenApi.BigDecimal(value) => value.toString()
      case OpenApi.BigInt(value)     => value.toString()
      case OpenApi.Boolean(value)    => SString.valueOf(value)
      case OpenApi.Double(value)     => SString.valueOf(value)
      case OpenApi.Float(value)      => SString.valueOf(value)
      case OpenApi.Int(value)        => SString.valueOf(value)
      case OpenApi.Long(value)       => SString.valueOf(value)
      case OpenApi.String(value)     => value

  sealed abstract class Number extends Primitive

  sealed abstract class Decimal extends Number

  final case class BigDecimal(value: SBigDecimal) extends Decimal
  final case class Double(value: SDouble) extends Decimal
  final case class Float(value: SFloat) extends Decimal

  sealed abstract class Integer extends Number

  final case class BigInt(value: SBigInt) extends Integer
  final case class Int(value: SInt) extends Integer
  final case class Long(value: SLong) extends Integer

  final case class Boolean(value: SBoolean) extends Primitive

  object Boolean:
    val True: OpenApi.Boolean = Boolean(true)
    val False: OpenApi.Boolean = Boolean(false)

  final case class String(value: SString) extends Primitive

  final case class Array[+A <: OpenApi](toVector: Vector[A]) extends Value:
    def isEmpty: SBoolean = toVector.isEmpty
    def get(index: SInt): Option[A] = toVector.lift(index)
    def ++[B >: A <: OpenApi](array: OpenApi.Array[B]): OpenApi.Array[B] = Array(toVector ++ array.toVector)
    def toChain: Chain[OpenApi] = Chain.fromSeq(toVector)

  object Array:
    val Empty: OpenApi.Array[Nothing] = OpenApi.Array(Vector.empty)
    def one[A <: OpenApi](value: A): OpenApi.Array[A] = Array(Vector(value))

  final case class Object(toSeqMap: SeqMap[SString, OpenApi]) extends Value:
    def toMap: Map[SString, OpenApi] = toSeqMap
    def isEmpty: SBoolean = toMap.isEmpty
    def contains(key: SString): SBoolean = toMap.contains(key)
    def get(key: SString): Option[OpenApi] = toMap.get(key)
    def getOrNull(key: SString): OpenApi = get(key).getOrElse(Null)
    def remove(key: SString): OpenApi.Object = Object(toSeqMap.removed(key))
    def keys: Set[SString] = toMap.keySet
    def values: List[OpenApi] = toMap.values.toList
    def ++(obj: OpenApi.Object): OpenApi.Object = Object(toSeqMap ++ obj.toSeqMap)
    def toChain: Chain[(SString, OpenApi)] = Chain.fromIterableOnce(toSeqMap)
    def toList: List[(SString, OpenApi)] = toMap.toList

    def deepMerge(obj: OpenApi.Object): OpenApi.Object = Object:
      obj.toSeqMap.foldLeft(this.toSeqMap) { case (result, (key, value)) =>
        result.updatedWith(key):
          case Some(current) => Some(current.merge(value))
          case None          => Some(value)
      }

  object Object:
    val Empty: OpenApi.Object = Object(SeqMap.empty)
    def one(key: SString, value: OpenApi): OpenApi.Object = Object(SeqMap(key -> value))

  def fromBigDecimal(value: SBigDecimal): OpenApi.Primitive = BigDecimal(value)
  def fromBigInt(value: SBigInt): OpenApi.Primitive = BigInt(value)
  def fromBoolean(value: SBoolean): OpenApi.Primitive = Boolean(value)
  def fromDouble(value: SDouble): OpenApi.Primitive = Double(value)
  def fromFloat(value: SFloat): OpenApi.Primitive = Float(value)
  def fromInt(value: SInt): OpenApi.Primitive = Int(value)
  def fromLong(value: SLong): OpenApi.Primitive = Long(value)
  def fromMap(values: Map[SString, OpenApi]): OpenApi.Object = Object(values.to(SeqMap))
  def fromSeqMap(values: SeqMap[SString, OpenApi]): OpenApi.Object = Object(values)
  def fromShort(value: Short): OpenApi.Primitive = fromInt(value.toInt)
  def fromByte(value: Byte): OpenApi.Primitive = fromInt(value.toInt)
  def fromString(value: SString): OpenApi.Primitive = String(value)
  def fromVector[A <: OpenApi](values: Vector[A]): OpenApi.Array[A] = Array(values)
  def fromChain[A <: OpenApi](values: Chain[A]): OpenApi.Array[A] = fromVector(values.toVector)
  def fromList[A <: OpenApi](values: List[A]): OpenApi.Array[A] = fromVector(values.toVector)
  def fromOption[A <: OpenApi](value: Option[A]): OpenApi.Null.type | A = value.getOrElse(Null)

  def arr[A <: OpenApi](values: A*): OpenApi.Array[A] = Array(values.toVector)
  def obj(values: (SString, OpenApi)*): OpenApi.Object = Object(values.to(SeqMap))
