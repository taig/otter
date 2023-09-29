package io.taig.otter

import cats.data.{Chain, Validated}
import cats.syntax.all.*
import io.taig.otter.validation.{Constraint, Validation, Violation, Violations}

sealed abstract class Primitive[A](description: Option[String], val format: Option[String])
    extends Schema[A](description)
    with Schema.Value[A]:
  self =>
  final override type Self[a] = Primitive[a]

  final override def description(f: Option[String] => Option[String]): Primitive[A] =
    Primitive(this, f(description), format)
  final def format(f: Option[String] => Option[String]): Primitive[A] = Primitive(this, description, f(format))
  final def format(value: Option[String]): Primitive[A] = format(_ => value)
  final def format(value: String): Primitive[A] = format(Some(value))

  final override def optional: Primitive[Option[A]] = new Primitive[Option[A]](description, format):
    export self.constraints
    override def isOptional: Boolean = true
    override def decode(data: Data): Validated[Violations, Option[A]] = data match
      case Data.Null => none.valid
      case _         => self.decode(data).map(_.some)
    override def encode(a: Option[A]): Data.Primitive | Data.Null.type = a.map(self.encode).getOrElse(Data.Null)
    override def parse(value: Option[String]): Validated[Violations, Option[A]] =
      value.fold(none.valid)(_ => self.parse(value).map(_.some))
    override def print(a: Option[A]): Option[String] = a.flatMap(self.print)

  final override def ivalidate[B](validation: Validation[A, B])(g: B => A): Primitive[B] =
    new Primitive[B](description, format):
      export self.isOptional
      override def constraints: Chain[Constraint] = self.constraints ++ validation.constraints
      override def decode(data: Data): Validated[Violations, B] =
        self.decode(data).andThen(validation(_).leftMap(Violations.root))
      override def encode(b: B): Data.Primitive | Data.Null.type = self.encode(g(b))
      override def parse(value: Option[String]): Validated[Violations, B] =
        self.parse(value).andThen(validation(_).leftMap(Violations.root))
      override def print(b: B): Option[String] = self.print(g(b))

  final def orElse[B](schema: Primitive[B]): Primitive[Either[A, B]] = new Primitive[Either[A, B]](description, format):
    override def constraints: Chain[Constraint] = Chain.empty
    override def isOptional: Boolean = false
    override def parse(value: Option[String]): Validated[Violations, Either[A, B]] =
      self.parse(value).map(_.asLeft).findValid(schema.parse(value).map(_.asRight))
    override def print(ab: Either[A, B]): Option[String] = ab.fold(self.print, schema.print)
    override def decode(data: Data): Validated[Violations, Either[A, B]] =
      self.decode(data).map(_.asLeft).findValid(schema.decode(data).map(_.asRight))
    override def encode(ab: Either[A, B]): Data.Primitive | Data.Null.type =
      ab.fold(self.encode, schema.encode)

  override def encode(a: A): Data.Primitive | Data.Null.type

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
    override def decode(data: Data): Validated[Violations, A] = tpe.decode(data)
    override def encode(a: A): Data.Primitive | Data.Null.type = tpe.encode(a)
    override def parse(value: Option[String]): Validated[Violations, A] = Validated
      .fromOption(value, Violations.rootNec(Violation.required))
      .andThen: value =>
        Validated.fromOption(
          tpe.parse(value),
          Violations.rootNec(Violation.tpe(tpe.name, actual = value))
        )
    override def print(a: A): Option[String] = Some(tpe.print(a))
