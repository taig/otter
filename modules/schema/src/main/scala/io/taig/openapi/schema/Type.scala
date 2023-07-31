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

  def decode(openapi: OpenApi.Primitive): Option[A] = (this, openapi) match
    case (Type.BigDecimal, OpenApi.BigDecimal(value)) => value.some
    case (Type.BigInt, OpenApi.BigInt(value))         => value.some
    case (Type.Boolean, OpenApi.Boolean(value))       => value.some
    case (Type.Double, OpenApi.Double(value))         => value.some
    case (Type.Float, OpenApi.Float(value))           => value.some
    case (Type.Int, OpenApi.Int(value))               => value.some
    case (Type.Long, OpenApi.Long(value))             => value.some
    case (Type.String, OpenApi.String(value))         => value.some
    case _                                            => none

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
    case BigDecimal => "bigDecimal"
    case BigInt     => "bigInt"
    case Boolean    => "boolean"
    case Double     => "double"
    case Float      => "float"
    case Int        => "int"
    case Long       => "long"
    case String     => "string"

object Type:
  given [A]: Show[Type[A]] = Show.fromToString
