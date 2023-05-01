package io.taig.openapi.schema

import cats.Show
import cats.syntax.all.*
import cats.data.Validated
import io.taig.openapi.OpenApi
import io.taig.openapi.validation.{Constraint, Violation}

enum Type[A]:
  self =>

  case BigDecimal extends Type[BigDecimal]
  case BigInt extends Type[BigInt]
  case Boolean extends Type[Boolean]
  case Double extends Type[Double]
  case Float extends Type[Float]
  case Int extends Type[Int]
  case Long extends Type[Long]
  case String extends Type[String]

  final def decode(openapi: OpenApi.Primitive): Validated[Violation[OpenApi, OpenApi], A] = (self, openapi) match
    case (Type.BigDecimal, OpenApi.BigDecimal(value)) => value.valid
    case (Type.BigInt, OpenApi.BigInt(value))         => value.valid
    case (Type.Boolean, OpenApi.Boolean(value))       => value.valid
    case (Type.Double, OpenApi.Double(value))         => value.valid
    case (Type.Float, OpenApi.Float(value))           => value.valid
    case (Type.Int, OpenApi.Int(value))               => value.valid
    case (Type.Long, OpenApi.Long(value))             => value.valid
    case (Type.String, OpenApi.String(value))         => value.valid
    case _ =>
      Violation(Constraint("type", reference = OpenApi.fromString(self.show).some), openapi).invalid

  def encode(a: A): OpenApi.Primitive = self match
    case Type.BigDecimal => OpenApi.fromBigDecimal(a)
    case Type.BigInt     => OpenApi.fromBigInt(a)
    case Type.Boolean    => OpenApi.fromBoolean(a)
    case Type.Double     => OpenApi.fromDouble(a)
    case Type.Float      => OpenApi.fromFloat(a)
    case Type.Int        => OpenApi.fromInt(a)
    case Type.Long       => OpenApi.fromLong(a)
    case Type.String     => OpenApi.fromString(a)

  def parse(value: String): Option[A] =
    val openapi = OpenApi.fromString(value)

    this match
      case Type.BigDecimal => openapi.toBigDecimal.map(_.value)
      case Type.BigInt     => openapi.toBigInt.map(_.value)
      case Type.Boolean    => openapi.toBoolean.map(_.value)
      case Type.Double     => openapi.toDouble.map(_.value)
      case Type.Float      => openapi.toFloat.map(_.value)
      case Type.Int        => openapi.toInt.map(_.value)
      case Type.Long       => openapi.toLong.map(_.value)
      case Type.String     => openapi.render.some

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
