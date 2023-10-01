package io.taig.otter.http

import cats.data.Validated
import cats.syntax.all.*
import io.taig.otter.validation.{Constraint, Violation, Violations}
import io.taig.otter.{Data, Value}

sealed abstract class Segment[A]:
  self =>
  def name: String
  def schema: Option[Value[?]]
  final def isOptional: Boolean = schema.exists(_.isOptional)

  final def imap[B](f: A => B)(g: B => A): Segment[B] = new Segment[B]:
    export self.{matches, name, print, schema}
    override def decode(a: Option[String]): Validated[Violations, B] = self.decode(a).map(f)
    override def encode(b: B): Option[String] = self.encode(g(b))

  def matches(value: String): Boolean

  def decode(a: Option[String]): Validated[Violations, A]
  def encode(a: A): Option[String]

  def print: String

  final def toPath: Path[A] = Path(this)

object Segment:
  def apply(static: String): Segment[Unit] = new Segment[Unit]:
    override def schema: Option[Value[?]] = none
    override def name: String = static
    override def matches(value: String): Boolean = value === static
    override def decode(a: Option[String]): Validated[Violations, Unit] = a match
      case Some(value) =>
        Validated.cond(
          matches(value),
          (),
          Violations.rootNec(Violation(Constraint.Equals(name), actual = Data.String(value)))
        )
      case None => Violations.rootNec(Violation.required).invalid
    override def encode(a: Unit): Option[String] = static.some
    override def print: String = static

  def apply[A](parameter: String, of: Value[A]): Segment[A] = new Segment[A]:
    override def schema: Option[Value[A]] = of.some
    override def name: String = parameter
    override def matches(value: String): Boolean = true
    override def decode(a: Option[String]): Validated[Violations, A] = of.parse(a)
    override def encode(a: A): Option[String] = of.print(a)
    override def print: String = s"{$parameter}"
