package io.taig.openapi.http

import cats.Eval
import cats.data.Validated
import cats.syntax.all.*
import io.taig.openapi.{History, OpenApi}
import io.taig.openapi.schema.{Value, Violations, Void}
import io.taig.openapi.validation.Constraint

sealed abstract class Segment[A]:
  def decode(openapi: OpenApi.Primitive): Validated[Violations, A]

  def encode(a: A): OpenApi.Primitive

object Segment:
  final private case class Static(name: String) extends Segment[Void]:
    override def decode(openapi: OpenApi.Primitive): Validated[Violations, Void] = Validated.cond(
      name === openapi.render,
      Void,
      Violations.oneNec(History.Root / name, Constraint.required.toViolation(openapi))
    )
    override def encode(a: Void): OpenApi.Primitive = OpenApi.fromString(name)

  final private case class Parameter[A](name: String, schema: Eval[Value[A]]) extends Segment[A]:
    override def decode(openapi: OpenApi.Primitive): Validated[Violations, A] =
      schema.value.decode(openapi).leftMap(_.modifyHistory(name /: _))
    override def encode(a: A): OpenApi.Primitive = schema.value.encode(a)
