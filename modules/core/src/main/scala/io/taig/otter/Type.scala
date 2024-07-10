package io.taig.otter

import cats.syntax.all.*

import java.lang.String as JString
import java.math.BigDecimal as JBigDecimal
import java.math.BigInteger as JBigInteger
import cats.data.Validated
import io.taig.otter.validation.Violations
import io.taig.otter.validation.Violation

enum Type[A]:
  case BigDecimal extends Type[JBigDecimal]
  case BigInteger extends Type[JBigInteger]
  case Boolean extends Type[Boolean]
  case Double extends Type[Double]
  case Float extends Type[Float]
  case Int extends Type[Int]
  case Long extends Type[Long]
  case String extends Type[JString]

  final def decode(data: Data.Primitive): Codec.Result[A] = (data, this) match
    case (Data.Boolean(value), Type.Boolean) => value.valid
    case (data: Data.Number, Type.Long) =>
      data.toLong.toValid(Violations.rootNec(Violation(Constraint.Type(name), actual = Data.String(data.name))))
    case (data: Data.Number, Type.Int) =>
      data.toInt.toValid(Violations.rootNec(Violation(Constraint.Type(name), actual = Data.String(data.name))))
    case (data: Data.Number, Type.Float)  => data.toFloat.valid
    case (data: Data.Number, Type.Double) => data.toDouble.valid
    case (data: Data.Number, Type.BigInteger) =>
      data.toBigInteger.toValid(Violations.rootNec(Violation(Constraint.Type(name), actual = Data.String(data.name))))
    case (data: Data.Number, Type.BigDecimal) => data.toBigDecimal.valid
    case (Data.String(value), Type.String)    => value.valid
    case (Data.String(value), _) =>
      Validated.fromOption(
        parse(value),
        Violations.rootNec(Violation(Constraint.Type(name), actual = Data.String("string")))
      )
    case (data, _) =>
      Violations.rootNec(Violation(Constraint.Type(name), actual = Data.String(data.name))).invalid

  final def encode(a: A): Data.Primitive = this match
    case Type.BigDecimal => Data.Number(a)
    case Type.BigInteger => Data.Number(a)
    case Type.Boolean    => Data.Boolean(a)
    case Type.Double     => Data.Number(a)
    case Type.Float      => Data.Number(a)
    case Type.Int        => Data.Number(a)
    case Type.Long       => Data.Number(a)
    case Type.String     => Data.String(a)

  final def parse(value: String): Option[A] = this match
    case Type.BigDecimal =>
      try Some(JBigDecimal(value))
      catch case _: NumberFormatException => None
    case Type.BigInteger =>
      try Some(JBigInteger(value))
      catch case _: NumberFormatException => None
    case Type.Boolean => value.toBooleanOption
    case Type.Double  => value.toDoubleOption
    case Type.Float   => value.toFloatOption
    case Type.Int     => value.toIntOption
    case Type.Long    => value.toLongOption
    case Type.String  => Some(value)

  final def print(a: A): String = this match
    case Type.BigDecimal => a.toString
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

  final override def toString: String = name
