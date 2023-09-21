package io.taig.otter.schema

import cats.data.{Chain, Validated}
import cats.syntax.all.*
import io.taig.otter.OpenApi
import io.taig.otter.validation.{Constraint, Validation, Violation}

sealed abstract class Primitive[A] extends Schema.Value[A]:
  self =>
  final override type Self[a] = Primitive[a]

  final def format(value: Option[String]): Primitive[A] = ???

  final def format(value: String): Primitive[A] = ???

  final override def optional: Primitive[Option[A]] = ???

  final override def ivalidate[B](validation: Validation[A, B])(g: B => A): Primitive[B] = ???

// object Primitive:
// val int: Primitive[Int] = new Primitive[Int]:
//   override def constraints: Chain[Constraint] = ???
//   override def isOptional: Boolean = ???

// def apply[A](of: Type[A]): Primitive[A] = new Primitive[A]:
//   override def constraints: Chain[Constraint] = Chain.empty
//   override def isOptional: Boolean = false
//   override def decode(openapi: Option[OpenApi.Value]): Validated[Violations, A] = openapi
//     .toValid(Violations.rootNec(Violation.required))
//     .andThen(value => decodeType(value).leftMap(Violations.rootNec))
//   def decodeType(openapi: OpenApi.Value): Validated[Violation, A] = of match
//     case Type.BigDecimal => openapi.as[BigDecimal]
//     case Type.BigInt     => openapi.as[BigInt]
//     case Type.Boolean    => openapi.as[Boolean]
//     case Type.Double     => openapi.as[Double]
//     case Type.Float      => openapi.as[Float]
//     case Type.Int        => openapi.as[Int]
//     case Type.Long       => openapi.as[Long]
//     case Type.String     => openapi.as[String]
//   override def encode(a: A): Option[OpenApi.Primitive] = encodeType(a).some
//   def encodeType(a: A): OpenApi.Primitive = of match
//     case Type.BigDecimal => OpenApi.Decimal(a)
//     case Type.BigInt     => OpenApi.Integer(a)
//     case Type.Boolean    => OpenApi.Boolean(a)
//     case Type.Double     => OpenApi.Decimal(a)
//     case Type.Float      => OpenApi.Decimal(a)
//     case Type.Int        => OpenApi.Integer(a)
//     case Type.Long       => OpenApi.Integer(a)
//     case Type.String     => OpenApi.String(a)
//   override def parse(value: Option[String]): Validated[Violations, A] = value
//     .toValid(Violations.rootNec(Violation.required))
//     .andThen(value => parseType(value).toValid(Violations.rootNec(Violation.tpe(of.toString, value))))
//   def parseType(value: String): Option[A] = of match
//     case Type.BigDecimal =>
//       try Some(BigDecimal(value))
//       catch case _: NumberFormatException => None
//     case Type.BigInt =>
//       try Some(BigInt(value))
//       catch case _: NumberFormatException => None
//     case Type.Boolean => value.toBooleanOption
//     case Type.Double  => value.toDoubleOption
//     case Type.Float   => value.toFloatOption
//     case Type.Int     => value.toIntOption
//     case Type.Long    => value.toLongOption
//     case Type.String  => Some(value)
//   override def print(a: A): Option[String] = Some(printType(a))
//   def printType(a: A): String = of match
//     case Type.BigDecimal => a.toString
//     case Type.BigInt     => a.toString
//     case Type.Boolean    => String.valueOf(a)
//     case Type.Double     => String.valueOf(a)
//     case Type.Float      => String.valueOf(a)
//     case Type.Int        => String.valueOf(a)
//     case Type.Long       => String.valueOf(a)
//     case Type.String     => a
