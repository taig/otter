package io.taig.otter.http

import cats.data.Validated
import cats.syntax.all.*
import io.taig.otter.schema.{History, Schema, Violations}
import io.taig.otter.syntax.*
import io.taig.otter.validation.{Constraint, Violation}

sealed abstract class Segment[A]:
  self =>
  def name: String
  def schema: Option[Schema.Value[?]]
  final def isOptional: Boolean = schema.exists(_.isOptional)

  final def imap[B](f: A => B)(g: B => A): Segment[B] = new Segment[B]:
    export self.{name, schema}
    override def parse(a: Option[String]): Validated[Violations, B] = self.parse(a).map(f)
    override def print(b: B): Option[String] = self.print(g(b))

  def parse(a: Option[String]): Validated[Violations, A]
  def print(a: A): Option[String]

  final def toPath: Path[A] = Path(this)

object Segment:
  def apply(static: String): Segment[Unit] = new Segment[Unit]:
    override def schema: Option[Schema.Value[?]] = none
    override def name: String = static
    override def parse(a: Option[String]): Validated[Violations, Unit] = a match
      case Some(a) =>
        Validated.cond(
          a === name,
          (),
          Violations.rootNec(Violation(Constraint.Equals(name), actual = a.asOpenApi.some))
        )
      case None => Violations.rootNec(Violation.required).invalid
    override def print(a: Unit): Option[String] = static.some

  def apply[A](parameter: String, of: => Schema.Value[A]): Segment[A] = new Segment[A]:
    override def schema: Option[Schema.Value[?]] = of.some
    override def name: String = parameter
    override def parse(a: Option[String]): Validated[Violations, A] = of.parse(a)
    override def print(a: A): Option[String] = of.print(a)
