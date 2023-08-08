package io.taig.otter.schema

import cats.data.{Chain, Validated}
import cats.syntax.all.*
import io.taig.enumeration.ext.Mapping
import io.taig.otter.OpenApi
import io.taig.otter.validation.{Constraint, Validation, Violation}

abstract class Enumeration[A] extends Schema.Value[A]:
  self =>
  final override type Self[a] = Enumeration[a]
  final override type Codec = OpenApi.Primitive
  final override type Properties[a] = Enumeration.Properties[a]

  def schema: Schema.Value[?]

  final override def copy(update: Enumeration.Properties[A]): Enumeration[A] = new Enumeration[A]:
    export self.{constraints, decode, encode, isOptional, parse, print, schema}
    override def properties: Enumeration.Properties[A] = update

  final override def optional: Enumeration[Option[A]] = new Enumeration[Option[A]]:
    export self.{constraints, isOptional, schema}
    override def properties: Enumeration.Properties[Option[A]] = self.properties.map(_.some)
    override def decode(openapi: OpenApi): Validated[Violations, Option[A]] = self.decode(openapi).map(_.some)
    override def encode(a: Option[A]): Option[OpenApi.Primitive] = a.flatMap(self.encode)
    override def parse(value: Option[String]): Validated[Violations, Option[A]] = self.parse(value).map(_.some)
    override def print(a: Option[A]): Option[String] = a.flatMap(self.print)

  final override def ivalidate[B](validation: Validation[A, B])(g: B => A): Enumeration[B] = new Enumeration[B]:
    export self.schema
    override def properties: Enumeration.Properties[B] = self.properties.flatMap(validation(_).toOption)
    override def constraints: Chain[Constraint] = self.constraints ++ validation.constraints
    override def isOptional: Boolean = true
    override def decode(openapi: OpenApi): Validated[Violations, B] =
      self.decode(openapi).andThen(validation(_).leftMap(Violations.root))
    override def encode(b: B): Option[OpenApi.Primitive] = self.encode(g(b))
    override def parse(value: Option[String]): Validated[Violations, B] =
      self.parse(value).andThen(validation(_).leftMap(Violations.root))
    override def print(b: B): Option[String] = self.print(g(b))

object Enumeration:
  final case class Properties[+A](description: Option[String], example: Option[A]) extends Schema.Properties[A]:
    override type Self[a] = Enumeration.Properties[a]
    override def modifyDescription(f: Option[String] => Option[String]): Enumeration.Properties[A] =
      copy(description = f(description))
    override def modifyExample[B](f: Option[A] => Option[B]): Enumeration.Properties[B] = copy(example = f(example))
    override def flatMap[B](f: A => Option[B]): Enumeration.Properties[B] = copy(example = example.flatMap(f))

  object Properties:
    val Default: Enumeration.Properties[Nothing] = Properties(None, None)

  def apply[A, B](of: => Schema.Value[A], mapping: Mapping[B, A]): Enumeration[B] = new Enumeration:
    override def schema: Schema.Value[A] = of
    override def properties: Enumeration.Properties[B] = Properties.Default
    override def constraints: Chain[Constraint] = Chain.empty
    override def isOptional: Boolean = false
    override def decode(openapi: OpenApi): Validated[Violations, B] = of
      .decode(openapi)
      .andThen: a =>
        mapping
          .prj(a)
          .toValid:
            val values = Chain.fromSeq(mapping.values).mapFilter(b => of.print(mapping.inj(b)))
            Violations.rootNec(Violation(Constraint.OneOf(values), openapi.toValue))
    override def encode(b: B): Option[OpenApi.Primitive] = of.encode(mapping.inj(b))
    override def parse(value: Option[String]): Validated[Violations, B] = of
      .parse(value)
      .andThen: a =>
        mapping
          .prj(a)
          .toValid:
            val values = Chain.fromSeq(mapping.values).mapFilter(b => of.print(mapping.inj(b)))
            Violations.rootNec(Violation(Constraint.OneOf(values), value.map(OpenApi.Text.apply)))

    override def print(b: B): Option[String] = of.print(mapping.inj(b))
