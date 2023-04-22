package io.taig.openapi.http

import cats.Eval
import cats.syntax.all.*
import cats.data.Validated
import io.taig.openapi.OpenApi
import io.taig.openapi.schema.{Schema, Violations}
import io.taig.validation.{identifiers, Violation}
import io.taig.validation.syntax.*

import java.nio.charset.StandardCharsets

sealed abstract class Segment[A]:
  def decode(openapi: OpenApi.Primitive): Validated[Violations, A]

  def encode(a: A): OpenApi.Primitive

  def print: String

  final def toPath: Path[A] = Path.one(this)

object Segment:
  final case class Value(value: String) extends Segment[Unit]:
    override def decode(openapi: OpenApi.Primitive): Validated[Violations, Unit] = Validated.cond(
      openapi.print === value,
      (),
      Violations
        .rootNec {
          identifiers.text.matches
            .toConstraint(reference = OpenApi.fromString(value).some)
            .toViolation(actual = openapi)
        }
        .modifyHistory(value /: _)
    )

    override def encode(a: Unit): OpenApi.Primitive = OpenApi.fromString(value)

    override def print: String = value

  final case class Parameter[A](name: String, schema: Eval[Schema.Of[A, OpenApi.Primitive]]) extends Segment[A]:
    def modifyName(f: String => String): Segment.Parameter[A] = copy(name = f(name))

    def modifySchema[B](f: Schema.Of[A, OpenApi.Primitive] => Schema.Of[B, OpenApi.Primitive]): Segment.Parameter[B] =
      copy(schema = schema.map(f))

    override def decode(openapi: OpenApi.Primitive): Validated[Violations, A] =
      schema.value.decode(openapi).leftMap(_.modifyHistory(name /: _))

    override def encode(a: A): OpenApi.Primitive = schema.value.encode(a)

    override def print: String = s"{$name}"
