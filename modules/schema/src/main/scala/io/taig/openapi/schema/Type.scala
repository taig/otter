package io.taig.openapi.schema

import cats.Show
import cats.data.{NonEmptyChain, Validated}
import cats.syntax.all.*
import io.taig.openapi.OpenApi
import io.taig.validation.{validations, Validation, Violation}

import scala.BigDecimal as SBigDecimal
import scala.BigInt as SBigInt

enum Type[A]:
  case BigDecimal extends Type[BigDecimal]
  case BigInt extends Type[BigInt]
  case Boolean extends Type[Boolean]
  case Double extends Type[Double]
  case Float extends Type[Float]
  case Int extends Type[Int]
  case Long extends Type[Long]
  case String extends Type[String]

  def decode(openapi: OpenApi.Primitive): Validated[Violations, A] = this
    .match {
      case Type.BigDecimal => refine(_.toBigDecimal.map(_.value))
      case Type.BigInt     => refine(_.toBigInt.map(_.value))
      case Type.Boolean    => refine(_.toBoolean.map(_.value))
      case Type.Double     => refine(_.toDouble.map(_.value))
      case Type.Float      => refine(_.toFloat.map(_.value))
      case Type.Int        => refine(_.toInt.map(_.value))
      case Type.Long       => refine(_.toLong.map(_.value))
      case Type.String     => refine(_.print.some)
    }
    .run(openapi)
    .leftMap(Violations.root)

  private def refine(f: OpenApi.Primitive => Option[A]): Validation[OpenApi, OpenApi.Primitive, OpenApi.Primitive, A] =
    validations.refine(show)(f).mapReference(OpenApi.fromString)

  def encode(a: A): OpenApi.Primitive = this match
    case Type.BigDecimal => OpenApi.fromBigDecimal(a)
    case Type.BigInt     => OpenApi.fromBigInt(a)
    case Type.Boolean    => OpenApi.fromBoolean(a)
    case Type.Double     => OpenApi.fromDouble(a)
    case Type.Float      => OpenApi.fromFloat(a)
    case Type.Int        => OpenApi.fromInt(a)
    case Type.Long       => OpenApi.fromLong(a)
    case Type.String     => OpenApi.fromString(a)

  def parse(value: String): Option[A] = this match
    case Type.BigDecimal =>
      try SBigDecimal(value).some
      catch case _: NumberFormatException => none
    case Type.BigInt =>
      try SBigInt(value).some
      catch case _: NumberFormatException => none
    case Type.Boolean =>
      value match
        case "true"  => true.some
        case "false" => false.some
        case _       => none
    case Type.Double => value.toDoubleOption
    case Type.Float  => value.toFloatOption
    case Type.Int    => value.toIntOption
    case Type.Long   => value.toLongOption
    case Type.String => value.some

  def show: String = this match
    case BigDecimal => "BigDecimal"
    case BigInt     => "BigInt"
    case Boolean    => "Boolean"
    case Double     => "Double"
    case Float      => "Float"
    case Int        => "Int"
    case Long       => "Long"
    case String     => "String"
