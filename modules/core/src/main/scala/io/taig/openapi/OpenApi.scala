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

  object BigDecimal:
    def parse(value: SString): Option[OpenApi.BigDecimal] =
      try BigDecimal(SBigDecimal(value)).some
      catch case _: NumberFormatException => none

  final case class Double(value: SDouble) extends Decimal

  object Double:
    def parse(value: SString): Option[OpenApi.Double] = value.toDoubleOption.map(apply)

  final case class Float(value: SFloat) extends Decimal

  object Float:
    def parse(value: SString): Option[OpenApi.Float] = value.toFloatOption.map(apply)

  sealed abstract class Integer extends Number

  final case class BigInt(value: SBigInt) extends Integer

  object BigInt:
    def parse(value: SString): Option[OpenApi.BigInt] =
      try BigInt(SBigInt(value)).some
      catch case _: NumberFormatException => none

  final case class Int(value: SInt) extends Integer

  object Int:
    def parse(value: SString): Option[OpenApi.Int] = value.toIntOption.map(apply)

  final case class Long(value: SLong) extends Integer

  object Long:
    def parse(value: SString): Option[OpenApi.Long] = value.toLongOption.map(apply)

  final case class Boolean(value: SBoolean) extends Primitive

  object Boolean:
    val True: OpenApi.Boolean = Boolean(true)
    val False: OpenApi.Boolean = Boolean(false)

    def parse(value: SString): Option[OpenApi.Boolean] = value.toBooleanOption.map(apply)

  final case class String(value: SString) extends Primitive

  final case class Array[+A <: OpenApi](toVector: Vector[A]) extends Value:
    def isEmpty: SBoolean = toVector.isEmpty
    def get(index: SInt): Option[A] = toVector.lift(index)
    def ++[B >: A <: OpenApi](array: OpenApi.Array[B]): OpenApi.Array[B] = Array(toVector ++ array.toVector)
    def toChain: Chain[A] = Chain.fromSeq(toVector)

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
