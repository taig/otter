package io.taig.openapi.schema

import cats.Show
import cats.syntax.all.*
import cats.data.Validated
import io.taig.openapi.OpenApi
import io.taig.openapi.validation.Violation

enum Type[A]:
  case BigDecimal extends Type[BigDecimal]
  case BigInt extends Type[BigInt]
  case Boolean extends Type[Boolean]
  case Double extends Type[Double]
  case Float extends Type[Float]
  case Int extends Type[Int]
  case Long extends Type[Long]
  case String extends Type[String]

  def decode(openapi: OpenApi.Primitive): Validated[Violation, A] = (this, openapi) match
    case (Type.BigDecimal, OpenApi.BigDecimal(value)) => value.valid
    case (Type.BigInt, OpenApi.BigInt(value))         => value.valid
    case (Type.Boolean, OpenApi.Boolean(value))       => value.valid
    case (Type.Double, OpenApi.Double(value))         => value.valid
    case (Type.Float, OpenApi.Float(value))           => value.valid
    case (Type.Int, OpenApi.Int(value))               => value.valid
    case (Type.Long, OpenApi.Long(value))             => value.valid
    case (Type.String, OpenApi.String(value))         => value.valid
    case _ =>
      Violation(identifier = "type", reference = OpenApi.fromString(toString).some, actual = openapi.some).invalid

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
    case Type.BigDecimal => OpenApi.BigDecimal.parse(value).map(_.value)
    case Type.BigInt     => OpenApi.BigInt.parse(value).map(_.value)
    case Type.Boolean    => OpenApi.Boolean.parse(value).map(_.value)
    case Type.Double     => OpenApi.Double.parse(value).map(_.value)
    case Type.Float      => OpenApi.Float.parse(value).map(_.value)
    case Type.Int        => OpenApi.Int.parse(value).map(_.value)
    case Type.Long       => OpenApi.Long.parse(value).map(_.value)
    case Type.String     => value.some

  def render(a: A): String = encode(a).render

  override def toString: String = this match
    case BigDecimal => "BigDecimal"
    case BigInt     => "BigInt"
    case Boolean    => "Boolean"
    case Double     => "Double"
    case Float      => "Float"
    case Int        => "Int"
    case Long       => "Long"
    case String     => "String"

object Type:
  given [A]: Show[Type[A]] = Show.fromToString
