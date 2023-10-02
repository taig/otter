package io.taig.otter.http

import cats.data.Validated
import cats.syntax.all.*
import io.taig.otter.validation.{Constraint, Violation, Violations}
import io.taig.otter.{Data, Union, Value}

sealed abstract class Segment[A]:
  self =>
  def name: String
  def schema: Option[Value.Required[?] | Union.Of[Value.Required[?], ?]]

  final def imap[B](f: A => B)(g: B => A): Segment[B] = new Segment[B]:
    export self.{matches, name, print, schema}
    override def decode(a: String): Validated[Violations, B] = self.decode(a).map(f)
    override def encode(b: B): String = self.encode(g(b))

  def matches(value: String): Boolean

  def decode(value: String): Validated[Violations, A]
  def encode(a: A): String

  def print: String

  final def toPath: Path[A] = Path(this)

object Segment:
  def apply(static: String): Segment[Unit] = new Segment[Unit]:
    override def schema: Option[Value.Required[?]] = none
    override def name: String = static
    override def matches(value: String): Boolean = value === static
    override def decode(value: String): Validated[Violations, Unit] = Validated.cond(
      matches(value),
      (),
      Violations.rootNec(Violation(Constraint.Equals(name), actual = Data.String(value)))
    )
    override def encode(a: Unit): String = static
    override def print: String = static

  def apply[A](parameter: String, of: Value.Required[A]): Segment[A] = new Segment[A]:
    override def schema: Option[Value.Required[A]] = of.some
    override def name: String = parameter
    override def matches(value: String): Boolean = true
    override def decode(value: String): Validated[Violations, A] = of.parse(value)
    override def encode(a: A): String = of.print(a)
    override def print: String = s"{$parameter}"

  def apply[A](parameter: String, of: Union.Required.Of[Value.Required[?], A]): Segment[A] = new Segment[A]:
    override def schema: Option[Union.Of[Value.Required[?], ?]] = of.some
    override def name: String = parameter
    override def matches(value: String): Boolean = true
    override def decode(value: String): Validated[Violations, A] = of.parse(value)
    override def encode(a: A): String = of.print(a)
    override def print: String = s"{$parameter}"
