package io.taig.otter

import cats.data.{Chain, Validated}
import cats.syntax.all.*
import io.taig.enumeration.ext.Mapping
import io.taig.otter.validation.{Constraint, Validation, Violation, Violations}

sealed abstract class Enumeration[A] extends Value[A]:
  self =>
  override type Self[a] <: Enumeration[a]
  final override type Optional[a] = Enumeration[a]

  def codec: Value[?]

  def values: Chain[Data.Primitive]

  final override def optional: Enumeration[Option[A]] =
    new Enumeration.Optional[Option[A]](self.description, self.codec):
      export self.values
      override def constraints: Chain[Constraint] = Chain.empty
      override def decode(data: Option[Data.Value]): Validated[Violations, Option[A]] =
        data.fold(none.valid)(_ => self.decode(data).map(_.some))
      override def encode(a: Option[A]): Data = a.map(self.encode).getOrElse(Data.Null)
      override def parse(value: Option[String]): Validated[Violations, Option[A]] =
        value.fold(none.valid)(_ => self.parse(value).map(_.some))
      override def print(a: Option[A]): Option[String] = a
        .map(self.print)
        .flatMap:
          case value: String         => value.some
          case value: Option[String] => value

object Enumeration:
  abstract class Required[A](val description: Option[String], val codec: Value.Required[?])
      extends Enumeration[A]
      with Value.Required[A]:
    self =>
    final override type Self[a] = Enumeration.Required[a]
    final override def isOptional: Boolean = false
    final override def description(f: Option[String] => Option[String]): Enumeration.Required[A] =
      new Required[A](f(description), codec) { export self.* }
    final override def ivalidate[B](validation: Validation[A, B])(g: B => A): Enumeration.Required[B] =
      new Required[B](description, codec):
        export self.values
        override def constraints: Chain[Constraint] = self.constraints ++ validation.constraints
        override def decode(data: Option[Data.Value]): Validated[Violations, B] =
          self.decode(data).andThen(validation(_).leftMap(Violations.root))
        override def encode(b: B): Data.Primitive = self.encode(g(b))
        override def parse(value: String): Validated[Violations, B] =
          self.parse(value).andThen(validation(_).leftMap(Violations.root))
        override def print(b: B): String = self.print(g(b))

  object Required:
    def apply[A, B](of: Value.Required[A], mapping: Mapping[B, A]): Enumeration.Required[B] = new Required[B](None, of):
      override def values: Chain[Data.Primitive] = Chain.fromSeq(mapping.values.map(encode))
      override def constraints: Chain[Constraint] = Chain.empty
      override def decode(data: Option[Data.Value]): Validated[Violations, B] = of
        .decode(data)
        .andThen: a =>
          Validated.fromOption(
            mapping.prj(a),
            Violations.rootNec(Violation(Constraint.OneOf(values), data.getOrElse(Data.Null)))
          )
      override def encode(b: B): Data.Primitive = of.encode(mapping.inj(b))
      override def parse(value: String): Validated[Violations, B] = of
        .parse(value)
        .andThen: a =>
          Validated.fromOption(
            mapping.prj(a),
            Violations.rootNec(Violation(Constraint.OneOf(values), Data.String(value)))
          )
      override def print(b: B): String = of.print(mapping.inj(b))

  abstract private class Optional[A](val description: Option[String], val codec: Value[?]) extends Enumeration[A]:
    self =>
    final override type Self[a] = Enumeration.Optional[a]
    final override def isOptional: Boolean = false
    final override def description(f: Option[String] => Option[String]): Enumeration.Optional[A] =
      new Enumeration.Optional[A](f(description), codec) { export self.* }
    final override def ivalidate[B](validation: Validation[A, B])(g: B => A): Enumeration.Optional[B] =
      new Enumeration.Optional[B](description, codec):
        export self.values
        override def constraints: Chain[Constraint] = self.constraints ++ validation.constraints
        override def decode(data: Option[Data.Value]): Validated[Violations, B] =
          self.decode(data).andThen(validation(_).leftMap(Violations.root))
        override def encode(b: B): Data = self.encode(g(b))
        override def parse(value: Option[String]): Validated[Violations, B] =
          self.parse(value).andThen(validation(_).leftMap(Violations.root))
        override def print(b: B): String | Option[String] = self.print(g(b))
