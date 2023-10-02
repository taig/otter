package io.taig.otter

import cats.data.{Chain, Validated}
import cats.syntax.all.*
import io.taig.otter.validation.{Constraint, Validation, Violation, Violations}

sealed abstract class Primitive[A] extends Codec[A] with Value[A]:
  self =>
  override type Self[a] <: Primitive[a]
  final override type Optional[a] = Primitive[a]

  def tpe: Type[?]

  def format: Option[String]
  def format(f: Option[String] => Option[String]): Self[A]
  final def format(value: Option[String]): Self[A] = format(_ => value)
  final def format(value: String): Self[A] = format(Some(value))

  final override def optional: Primitive[Option[A]] =
    new Primitive.Optional[Option[A]](self.description, self.format, self.tpe):
      override def constraints: Chain[Constraint] = Chain.empty
      override def decode(data: Option[Data.Value]): Validated[Violations, Option[A]] =
        data.fold(none.valid)(_ => self.decode(data).map(_.some))
      override def encode(a: Option[A]): Data.Primitive | Data.Null.type = a.map(self.encode).getOrElse(Data.Null)
      override def parse(value: Option[String]): Validated[Violations, Option[A]] =
        value.fold(none.valid)(_ => self.parse(value).map(_.some))
      override def print(a: Option[A]): Option[String] = a
        .map(self.print)
        .flatMap:
          case value: String         => value.some
          case value: Option[String] => value

  override def encode(a: A): Data.Primitive | Data.Null.type

object Primitive:
  abstract class Required[A](val description: Option[String], val format: Option[String], val tpe: Type[?])
      extends Primitive[A]
      with Value.Required[A]:
    self =>
    final override type Self[a] = Primitive.Required[a]

    final override def isOptional: Boolean = false

    final override def description(f: Option[String] => Option[String]): Primitive.Required[A] =
      new Required[A](f(description), format, tpe) { export self.* }

    final override def format(f: Option[String] => Option[String]): Primitive.Required[A] =
      new Required[A](description, f(format), tpe) { export self.* }

    final override def ivalidate[B](validation: Validation[A, B])(g: B => A): Primitive.Required[B] =
      new Required[B](description, format, tpe):
        override def constraints: Chain[Constraint] = self.constraints ++ validation.constraints
        override def decode(data: Option[Data.Value]): Validated[Violations, B] =
          self.decode(data).andThen(validation(_).leftMap(Violations.root))
        override def encode(b: B): Data.Primitive = self.encode(g(b))
        override def parse(value: String): Validated[Violations, B] =
          self.parse(value).andThen(validation(_).leftMap(Violations.root))
        override def print(b: B): String = self.print(g(b))

    override def encode(a: A): Data.Primitive

  object Required:
    def apply[A](of: Type[A]): Primitive.Required[A] = new Required[A](None, None, of):
      override def constraints: Chain[Constraint] = Chain.empty
      override def decode(data: Option[Data.Value]): Validated[Violations, A] = data match
        case Some(data: Data.Primitive) => of.decode(data)
        case Some(data)                 => Violations.rootNec(Violation.tpe(of.name, actual = data.name)).invalid
        case None                       => Violations.rootNec(Violation.required).invalid
      override def encode(a: A): Data.Primitive = of.encode(a)
      override def parse(value: String): Validated[Violations, A] = Validated.fromOption(
        of.parse(value),
        Violations.rootNec(Violation.tpe(of.name, actual = value))
      )
      override def print(a: A): String = of.print(a)

  abstract private class Optional[A](val description: Option[String], val format: Option[String], val tpe: Type[?])
      extends Primitive[A]:
    self =>
    final override type Self[a] = Primitive.Optional[a]

    final override def isOptional: Boolean = true

    final override def description(f: Option[String] => Option[String]): Primitive.Optional[A] =
      new Primitive.Optional[A](f(description), format, tpe) { export self.* }

    final override def format(f: Option[String] => Option[String]): Primitive.Optional[A] =
      new Primitive.Optional[A](description, f(format), tpe) { export self.* }

    final override def ivalidate[B](validation: Validation[A, B])(g: B => A): Primitive.Optional[B] =
      new Primitive.Optional[B](description, format, tpe):
        override def constraints: Chain[Constraint] = self.constraints ++ validation.constraints
        override def decode(data: Option[Data.Value]): Validated[Violations, B] =
          self.decode(data).andThen(validation(_).leftMap(Violations.root))
        override def encode(b: B): Data.Primitive | Data.Null.type = self.encode(g(b))
        override def parse(value: Option[String]): Validated[Violations, B] =
          self.parse(value).andThen(validation(_).leftMap(Violations.root))
        override def print(b: B): String | Option[String] = self.print(g(b))

    override def encode(a: A): Data.Primitive | Data.Null.type
