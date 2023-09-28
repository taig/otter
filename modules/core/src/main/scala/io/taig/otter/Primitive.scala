package io.taig.otter

import cats.data.{Chain, Validated}
import cats.syntax.all.*
import io.taig.otter.validation.{Constraint, Validation, Violation, Violations}

sealed abstract class Primitive[A](description: Option[String], val format: Option[String])
    extends Schema[A](description)
    with Value[A]:
  self =>
  final override type Self[a] = Primitive[a]

  final override def description(f: Option[String] => Option[String]): Primitive[A] =
    Primitive(this, f(description), format)
  final def format(f: Option[String] => Option[String]): Primitive[A] = Primitive(this, description, f(format))
  final def format(value: Option[String]): Primitive[A] = format(_ => value)
  final def format(value: String): Primitive[A] = format(Some(value))

  final override def optional: Primitive[Option[A]] = ???

  final override def ivalidate[B](validation: Validation[A, B])(g: B => A): Primitive[B] = ???

  final def orElse[B](schema: Primitive[B]): Primitive[Either[A, B]] = ???

object Primitive:
  extension [A <: Matchable](self: Primitive[A])
    inline def |[B <: Matchable](schema: Primitive[B]): Primitive[A | B] = self
      .orElse(schema)
      .imap {
        case Left(a)  => a
        case Right(b) => b
      } {
        case a: A => Left(a)
        case b: B => Right(b)
      }

  def apply[A](schema: Primitive[A], description: Option[String], format: Option[String]): Primitive[A] =
    new Primitive[A](description, format) { export schema.* }

  def apply[A](tpe: Type[A]): Primitive[A] = new Primitive[A](None, None):
    override def constraints: Chain[Constraint] = Chain.empty
    override def isOptional: Boolean = false
    override def decode(data: Data): Validated[Violations, A] = (data, tpe) match
      case (Data.Boolean(value), Type.Boolean)               => value.valid
      case (Data.String(value), Type.String)                 => value.valid
      case (Data.Number(value: Int), Type.Int)               => value.valid
      case (Data.Number(value: Long), Type.Long)             => value.valid
      case (Data.Number(value: Double), Type.Double)         => value.valid
      case (Data.Number(value: Float), Type.Float)           => value.valid
      case (Data.Number(value: BigDecimal), Type.BigDecimal) => value.valid
      case (Data.Number(value: BigInt), Type.BigInt)         => value.valid
      case (Data.Null, _)                                    => Violations.rootNec(Violation.required).invalid
      case (data, _) => Violations.rootNec(Violation.tpe(tpe.name, actual = data.name)).invalid
    override def encode(a: A): Data = tpe match
      case Type.BigDecimal => Data.Number(a)
      case Type.BigInt     => Data.Number(a)
      case Type.Boolean    => Data.Boolean(a)
      case Type.Double     => Data.Number(a)
      case Type.Float      => Data.Number(a)
      case Type.Int        => Data.Number(a)
      case Type.Long       => Data.Number(a)
      case Type.String     => Data.String(a)
    override def parse(value: Option[String]): Validated[Violations, A] = Validated
      .fromOption(value, Violations.rootNec(Violation.required))
      .andThen: value =>
        val result: Option[A] = tpe match
          case Type.BigDecimal =>
            try Some(BigDecimal(value))
            catch case _: NumberFormatException => None
          case Type.BigInt =>
            try Some(BigInt(value))
            catch case _: NumberFormatException => None
          case Type.Boolean => value.toBooleanOption
          case Type.Double  => value.toDoubleOption
          case Type.Float   => value.toFloatOption
          case Type.Int     => value.toIntOption
          case Type.Long    => value.toLongOption
          case Type.String  => Some(value)
        Validated.fromOption(
          result,
          Violations.rootNec(Violation.tpe(tpe.name, actual = value))
        )
    override def print(a: A): Option[String] =
      val result = tpe match
        case Type.BigDecimal => a.toString
        case Type.BigInt     => a.toString
        case Type.Boolean    => String.valueOf(a)
        case Type.Double     => String.valueOf(a)
        case Type.Float      => String.valueOf(a)
        case Type.Int        => String.valueOf(a)
        case Type.Long       => String.valueOf(a)
        case Type.String     => String.valueOf(a)
      Some(result)
