package io.taig.openapi.schema

import cats.data.{Chain, Validated}
import io.taig.openapi.OpenApi
import io.taig.openapi.validation.{Constraint, Validation}

abstract class Schema[A]:
  self =>

  type Self[a] <: Schema[a] { type Self[a] = self.Self[a] }

  def constraints: Chain[Constraint]

  def imap[B](f: A => B)(g: B => A): Self[B]
  final def const(value: A): Self[Unit] = imap(_ => ())(_ => value)

object Schema:
  abstract class Value[A] extends Schema[A]:
    self =>
    override type Self[a] <: Value[a] { type Self[a] = self.Self[a] }
