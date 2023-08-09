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

  final def imap[B](f: A => B)(g: B => A): Segment[B] = new Segment[B]:
    export self.{name, schema}
    override def decode(a: String): Validated[Violations, B] = self.decode(a).map(f)
    override def encode(b: B): Option[String] = self.encode(g(b))

  def decode(a: String): Validated[Violations, A]
  def encode(a: A): Option[String]

  final def toPath: Path[A] = Path(this)

object Segment:
  def apply(static: String): Segment[Unit] = new Segment[Unit]:
    override def schema: Option[Schema.Value[?]] = none
    override def name: String = static
    override def decode(a: String): Validated[Violations, Unit] = Validated.cond(
      a === name,
      (),
      Violations.oneNec(History.Root / name, Violation(Constraint.Equals(name), actual = a.asOpenApi.some))
    )
    override def encode(a: Unit): Option[String] = static.some

  def apply[A](parameter: String, of: => Schema.Value[A]): Segment[A] = new Segment[A]:
    override def schema: Option[Schema.Value[?]] = of.some
    override def name: String = parameter
    override def decode(a: String): Validated[Violations, A] = of.parse(a.some)
    override def encode(a: A): Option[String] = of.print(a)
