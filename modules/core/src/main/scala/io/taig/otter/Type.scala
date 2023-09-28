package io.taig.otter

import cats.data.Validated
import cats.syntax.all.*
import io.taig.otter.validation.{Violation, Violations}

import scala.BigInt as SBigInt
import scala.BigDecimal as SBigDecimal
import java.lang.String as JString

enum Type[A]:
  case BigDecimal extends Type[SBigDecimal]
  case BigInt extends Type[SBigInt]
  case Boolean extends Type[Boolean]
  case Double extends Type[Double]
  case Float extends Type[Float]
  case Int extends Type[Int]
  case Long extends Type[Long]
  case String extends Type[JString]

  def decode(data: Data): Validated[Violations, A] = (data, this) match
    case (Data.Boolean(value), Type.Boolean)               => value.valid
    case (Data.Number(value: Int), Type.Int)               => value.valid
    case (Data.Number(value: Long), Type.Long)             => value.valid
    case (Data.Number(value: Double), Type.Double)         => value.valid
    case (Data.Number(value: Float), Type.Float)           => value.valid
    case (Data.Number(value: BigDecimal), Type.BigDecimal) => value.valid
    case (Data.Number(value: BigInt), Type.BigInt)         => value.valid
    case (Data.String(value), Type.String)                 => value.valid
    case (Data.String(value), _) =>
      Validated.fromOption(parse(value), Violations.rootNec(Violation.tpe(name, "string")))
    case (Data.Null, _) => Violations.rootNec(Violation.required).invalid
    case (data, _)      => Violations.rootNec(Violation.tpe(name, actual = data.name)).invalid

  def encode(a: A): Data.Primitive = this match
    case Type.BigDecimal => Data.Number(a)
    case Type.BigInt     => Data.Number(a)
    case Type.Boolean    => Data.Boolean(a)
    case Type.Double     => Data.Number(a)
    case Type.Float      => Data.Number(a)
    case Type.Int        => Data.Number(a)
    case Type.Long       => Data.Number(a)
    case Type.String     => Data.String(a)

  def parse(value: String): Option[A] = this match
    case Type.BigDecimal =>
      try Some(SBigDecimal(value))
      catch case _: NumberFormatException => None
    case Type.BigInt =>
      try Some(SBigInt(value))
      catch case _: NumberFormatException => None
    case Type.Boolean => value.toBooleanOption
    case Type.Double  => value.toDoubleOption
    case Type.Float   => value.toFloatOption
    case Type.Int     => value.toIntOption
    case Type.Long    => value.toLongOption
    case Type.String  => Some(value)

  def print(a: A): String = this match
    case Type.BigDecimal => a.toString
    case Type.BigInt     => a.toString
    case Type.Boolean    => JString.valueOf(a)
    case Type.Double     => JString.valueOf(a)
    case Type.Float      => JString.valueOf(a)
    case Type.Int        => JString.valueOf(a)
    case Type.Long       => JString.valueOf(a)
    case Type.String     => JString.valueOf(a)

  final def name: String = this match
    case BigDecimal => "bigDecimal"
    case BigInt     => "bigInt"
    case Boolean    => "boolean"
    case Double     => "double"
    case Float      => "float"
    case Int        => "int"
    case Long       => "long"
    case String     => "string"
