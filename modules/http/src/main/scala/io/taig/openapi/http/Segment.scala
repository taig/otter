package io.taig.openapi.http

import cats.Eval
import cats.data.Validated
import cats.syntax.all.*
import io.taig.openapi.History
import io.taig.openapi.schema.{Schema, Violations, Void}
import io.taig.openapi.syntax.*
import io.taig.openapi.validation.Constraint

sealed abstract class Segment[A]:
  def name: String
  def matches(segment: String): Boolean
  def decode(value: String): Validated[Violations, A]
  def encode(a: A): String
  def print: String
  final def toPath: Path[A] = Path(this)

object Segment:
  final case class Static(name: String) extends Segment[Void]:
    override def matches(segment: String): Boolean = name === segment
    override def decode(value: String): Validated[Violations, Void] = Validated.cond(
      matches(value),
      Void,
      Violations.oneNec(
        History.Root / name,
        Constraint.required.toViolation(value.asOpenApi)
      )
    )
    override def encode(a: Void): String = name
    override def print: String = name

  final case class Parameter[A](name: String, schema: Eval[Schema.Value[A]]) extends Segment[A]:
    override def matches(segment: String): Boolean = true
    override def decode(value: String): Validated[Violations, A] =
      schema.value.parse(value).leftMap(_.modifyHistory(name /: _))
    override def encode(a: A): String = schema.value.render(a)
    override def print: String = s"{$name}"
