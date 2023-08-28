package io.taig.otter.schema

import cats.data.{Chain, Validated}
import cats.syntax.all.*
import io.taig.enumeration.ext.Mapping
import io.taig.otter.OpenApi
import io.taig.otter.validation.{Constraint, Validation, Violation}

abstract class Enumeration[A] extends Schema.Value[A]:
  self =>
  final override type Self[a] = Enumeration[a]

  def values: Chain[OpenApi]

  final override def optional: Enumeration[Option[A]] = new Enumeration[Option[A]]:
    export self.{constraints, isOptional, values}
    override def decode(openapi: Option[OpenApi.Value]): Validated[Violations, Option[A]] =
      openapi.traverse(self.decode)
    override def encode(a: Option[A]): Option[OpenApi.Primitive] = a.flatMap(self.encode)
    override def parse(value: Option[String]): Validated[Violations, Option[A]] = self.parse(value).map(_.some)
    override def print(a: Option[A]): Option[String] = a.flatMap(self.print)

  final override def ivalidate[B](validation: Validation[A, B])(g: B => A): Enumeration[B] = new Enumeration[B]:
    export self.{ values}
    override def constraints: Chain[Constraint] = self.constraints ++ validation.constraints
    override def isOptional: Boolean = true
    override def decode(openapi: Option[OpenApi.Value]): Validated[Violations, B] =
      self.decode(openapi).andThen(validation(_).leftMap(Violations.root))
    override def encode(b: B): Option[OpenApi.Primitive] = self.encode(g(b))
    override def parse(value: Option[String]): Validated[Violations, B] =
      self.parse(value).andThen(validation(_).leftMap(Violations.root))
    override def print(b: B): Option[String] = self.print(g(b))

object Enumeration:
  def apply[A, B](of: => Schema.Value[A], mapping: Mapping[B, A]): Enumeration[B] = new Enumeration:
    override def schema: Schema.Value[A] = of
    override def values: Chain[OpenApi] =
      Chain.fromSeq(mapping.values).map(mapping.inj).map(of.encode(_).getOrElse(OpenApi.Null))
    override def constraints: Chain[Constraint] = Chain.empty
    override def isOptional: Boolean = false
    override def decode(openapi: Option[OpenApi.Value]): Validated[Violations, B] = of
      .decode(openapi)
      .andThen: a =>
        mapping
          .prj(a)
          .toValid:
            val values = Chain.fromSeq(mapping.values).mapFilter(b => of.print(mapping.inj(b)))
            Violations.rootNec(Violation(Constraint.OneOf(values), openapi.getOrElse(OpenApi.Null)))
    override def encode(b: B): Option[OpenApi.Primitive] = of.encode(mapping.inj(b))
    override def parse(value: Option[String]): Validated[Violations, B] = of
      .parse(value)
      .andThen: a =>
        mapping
          .prj(a)
          .toValid:
            val values = Chain.fromSeq(mapping.values).mapFilter(b => of.print(mapping.inj(b)))
            Violations.rootNec(
              Violation(Constraint.OneOf(values), value.map(OpenApi.String.apply).getOrElse(OpenApi.Null))
            )

    override def print(b: B): Option[String] = of.print(mapping.inj(b))
