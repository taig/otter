package io.taig.openapi.schema

import cats.Show
import cats.syntax.all.*
import cats.data.Validated
import io.taig.openapi.OpenApi
import io.taig.validation.{validations, Constraint, Validation, Violation}

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

  def decode(openapi: OpenApi.Primitive): Validated[Violations, A] =
    def refine(f: OpenApi.Primitive => Option[A]) =
      validations.refine(self.toString)(f).mapReference(OpenApi.fromString)

    (self, openapi) match
      case (Type.Int, OpenApi.Int(value))       => value.valid
      case (Type.String, OpenApi.String(value)) => value.valid
      case _ =>
        Violations
          .rootNec(Violation(Constraint("type", reference = OpenApi.fromString(self.show).some), openapi))
          .invalid
//
//    self.match {
//      case Type.BigDecimal => refine(_.toBigDecimal.map(_.value))
//      case Type.BigInt     => refine(_.toBigInt.map(_.value))
//      case Type.Boolean    => refine(_.toBoolean.map(_.value))
//      case Type.Double     => refine(_.toDouble.map(_.value))
//      case Type.Float      => refine(_.toFloat.map(_.value))
//      case Type.Int        => refine(_.toInt.map(_.value))
//      case Type.Long       => refine(_.toLong.map(_.value))
//      case Type.String     => refine(_.print.some)
//    }.run(openapi).leftMap(Violations.root)

  def encode(a: A): OpenApi.Primitive = self match
    case Type.BigDecimal => OpenApi.fromBigDecimal(a)
    case Type.BigInt     => OpenApi.fromBigInt(a)
    case Type.Boolean    => OpenApi.fromBoolean(a)
    case Type.Double     => OpenApi.fromDouble(a)
    case Type.Float      => OpenApi.fromFloat(a)
    case Type.Int        => OpenApi.fromInt(a)
    case Type.Long       => OpenApi.fromLong(a)
    case Type.String     => OpenApi.fromString(a)

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
