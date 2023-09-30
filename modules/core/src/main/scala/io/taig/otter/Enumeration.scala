package io.taig.otter

import cats.syntax.all.*
import cats.data.{Chain, Validated}
import io.taig.enumeration.ext.Mapping
import io.taig.otter.validation.{Constraint, Validation, Violation, Violations}

sealed abstract class Enumeration[A](description: Option[String]) extends Schema[A](description) with Schema.Value[A]:
  final override type Self[a] = Enumeration[a]
  final override def description(f: Option[String] => Option[String]): Enumeration[A] =
    Enumeration(this, f(description))

  final override def optional: Enumeration[Option[A]] = ???

  final override def ivalidate[B](validation: Validation[A, B])(g: B => A): Enumeration[B] = ???

object Enumeration:
  def apply[A](schema: Enumeration[A], description: Option[String]): Enumeration[A] =
    new Enumeration[A](description) { export schema.* }

  def apply[A, B](schema: Schema.Value[A], mapping: Mapping[B, A]): Enumeration[B] = new Enumeration[B](None):
    def values: Chain[String] = Chain.fromSeq(mapping.values.mapFilter(print))
    override def constraints: Chain[Constraint] = Chain.empty
    override def isOptional: Boolean = false
    override def decode(data: Option[Data.Value]): Validated[Violations, B] = schema
      .decode(data)
      .andThen: a =>
        Validated.fromOption(
          mapping.prj(a),
          Violations.rootNec(Violation(Constraint.OneOf(values), data.getOrElse(Data.Null)))
        )
    override def encode(b: B): Data = schema.encode(mapping.inj(b))
    override def parse(value: Option[String]): Validated[Violations, B] = schema
      .parse(value)
      .andThen: a =>
        Validated.fromOption(
          mapping.prj(a),
          Violations.rootNec(Violation(Constraint.OneOf(values), value.map(Data.String.apply).getOrElse(Data.Null)))
        )
    override def print(b: B): Option[String] = schema.print(mapping.inj(b))
