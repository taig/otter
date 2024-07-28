package io.taig.otter

import cats.syntax.all.*

import java.lang.String as JString
import java.math.BigDecimal as JBigDecimal
import java.math.BigInteger as JBigInteger

enum Type[A]:
  case BigDecimal extends Type[JBigDecimal]
  case BigInteger extends Type[JBigInteger]
  case Boolean extends Type[Boolean]
  case Double extends Type[Double]
  case Float extends Type[Float]
  case Int extends Type[Int]
  case Long extends Type[Long]
  case String extends Type[JString]

  def decode(data: Data.Primitive): Option[A] = (data, this) match
    case (Data.Boolean(value), Type.Boolean)  => value.some
    case (data: Data.Number, Type.Long)       => data.toLong
    case (data: Data.Number, Type.Int)        => data.toInt
    case (data: Data.Number, Type.Float)      => data.toFloat
    case (data: Data.Number, Type.Double)     => data.toDouble
    case (data: Data.Number, Type.BigInteger) => data.toBigInteger
    case (data: Data.Number, Type.BigDecimal) => data.toBigDecimal
    case (Data.String(value), Type.String)    => value.some
    case (Data.String(value), _)              => parse(value)
    case (data, _)                            => none

  def encode(a: A): Data.Primitive = this match
    case Type.BigDecimal => Data.Number(a)
    case Type.BigInteger => Data.Number(a)
    case Type.Boolean    => Data.Boolean(a)
    case Type.Double     => Data.Number(a)
    case Type.Float      => Data.Number(a)
    case Type.Int        => Data.Number(a)
    case Type.Long       => Data.Number(a)
    case Type.String     => Data.String(a)

  def parse(value: String): Option[A] = this match
    case Type.BigDecimal =>
      try Some(new JBigDecimal(value))
      catch case _: NumberFormatException => None
    case Type.BigInteger =>
      try Some(new JBigInteger(value))
      catch case _: NumberFormatException => None
    case Type.Boolean => value.toBooleanOption
    case Type.Double  => value.toDoubleOption
    case Type.Float   => value.toFloatOption
    case Type.Int     => value.toIntOption
    case Type.Long    => value.toLongOption
    case Type.String  => Some(value)

  def print(a: A): String = this match
    case Type.BigDecimal => a.toPlainString
    case Type.BigInteger => a.toString
    case Type.Boolean    => JString.valueOf(a)
    case Type.Double     => JString.valueOf(a)
    case Type.Float      => JString.valueOf(a)
    case Type.Int        => JString.valueOf(a)
    case Type.Long       => JString.valueOf(a)
    case Type.String     => JString.valueOf(a)

  final def name: String = this match
    case Type.BigDecimal | Type.BigInteger | Type.Double | Type.Float | Type.Int | Type.Long => "number"
    case Type.Boolean                                                                        => "boolean"
    case Type.String                                                                         => "string"

  final override def toString: JString = name
