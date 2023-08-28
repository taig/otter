package io.taig.otter.schema

import cats.data.{Chain, Validated}
import cats.syntax.all.*
import io.taig.otter.OpenApi
import io.taig.otter.validation.{Constraint, Validation, Violation}

sealed abstract class Dynamic[A] extends Schema[A]:
  self =>
  final override type Self[a] = Dynamic[a]

  final override def optional: Dynamic[Option[A]] = new Dynamic[Option[A]]:
    export self.constraints
    override def isOptional: Boolean = true
    override def decode(openapi: Option[OpenApi.Value]): Validated[Violations, Option[A]] =
      openapi.traverse(self.decode)
    override def encode(a: Option[A]): Option[OpenApi.Value] = a.flatMap(self.encode)

  final override def ivalidate[B](validation: Validation[A, B])(g: B => A): Dynamic[B] = new Dynamic[B]:
    export self.isOptional
    override def constraints: Chain[Constraint] = self.constraints ++ validation.constraints
    override def decode(openapi: Option[OpenApi.Value]): Validated[Violations, B] =
      self.decode(openapi).andThen(validation(_).leftMap(Violations.root))
    override def encode(b: B): Option[OpenApi.Value] = self.encode(g(b))

object Dynamic:
  val Value: Dynamic[OpenApi.Value] = new Dynamic[OpenApi.Value]:
    override def constraints: Chain[Constraint] = Chain.empty
    override def isOptional: Boolean = false
    override def decode(openapi: Option[OpenApi.Value]): Validated[Violations, OpenApi.Value] =
      openapi.toValid(Violations.rootNec(Violation.required))
    override def encode(a: OpenApi.Value): Option[OpenApi.Value] = a.some
