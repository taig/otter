package io.taig.otter

import cats.data.{Chain, Validated}
import cats.syntax.all.*
import io.taig.otter.validation.{Constraint, Validation, Violation, Violations}

sealed abstract class Primitive[A](description: Option[String]) extends Value[A](description):
  self =>
  final override type Self[a] = Primitive[a]

  final override def description(f: Option[String] => Option[String]): Primitive[A] =
    new Primitive[A](f(description)) { export self.* }

  final override def optional: Primitive[Option[A]] = new Primitive[Option[A]](description):
    override def constraints: Chain[Constraint] = self.constraints
    override def isOptional: Boolean = true
    override def decode(data: Data): Validated[Violations, Option[A]] = data match
      case Data.Null => none.valid
      case data      => self.decode(data).map(_.some)
    override def encode(a: Option[A]): Data = a.map(self.encode).getOrElse(Data.Null)
    override def parse(value: Option[String]): Validated[Violations, Option[A]] =
      value.fold(none.valid)(value => self.parse(value.some).map(_.some))
    override def print(a: Option[A]): Option[String] = a.flatMap(self.print)

  override def ivalidate[B](validation: Validation[A, B])(g: B => A): Primitive[B] = new Primitive[B](description):
    override def constraints: Chain[Constraint] = self.constraints ++ validation.constraints
    override def isOptional: Boolean = self.isOptional
    override def decode(data: Data): Validated[Violations, B] =
      self.decode(data).andThen(validation(_).leftMap(Violations.root))
    override def encode(b: B): Data = self.encode(g(b))
    override def parse(value: Option[String]): Validated[Violations, B] =
      self.parse(value).andThen(validation(_).leftMap(Violations.root))
    override def print(b: B): Option[String] = self.print(g(b))

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

  def apply[A](tpe: Type[A], description: Option[String]): Primitive[A] = new Primitive[A](description):
    override def constraints: Chain[Constraint] = Chain.empty
    override def isOptional: Boolean = false
    override def decode(data: Data): Validated[Violations, A] = (data, tpe) match
      case (Data.Boolean(value), Type.Boolean) => value.valid
      case (Data.String(value), Type.String)   => value.valid
      case (Data.Null, _)                      => Violations.rootNec(Violation.required).invalid
      case (data, _) => Violations.rootNec(Violation.tpe(tpe.name, actual = data.name)).invalid
    override def encode(a: A): Data = ???
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
