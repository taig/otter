package io.taig.otter

import cats.data.{Chain, Validated}
import cats.syntax.all.*
import io.taig.otter.validation.{Constraint, Validation, Violation, Violations}

sealed abstract class Dynamic[A](description: Option[String]) extends Schema[A](description):
  self =>
  final override type Self[a] = Dynamic[a]

  final override def description(f: Option[String] => Option[String]): Dynamic[A] = Dynamic(this, f(description))

  final override def optional: Dynamic[Option[A]] = new Dynamic[Option[A]](description):
    export self.constraints
    override def isOptional: Boolean = true
    override def encode(a: Option[A]): Data = a.map(self.encode).getOrElse(Data.Null)
    override def decode(data: Option[Data.Value]): Validated[Violations, Option[A]] =
      data.fold(none.valid)(_ => self.decode(data).map(_.some))

  final override def ivalidate[B](validation: Validation[A, B])(g: B => A): Dynamic[B] = new Dynamic[B](description):
    export self.isOptional
    override def constraints: Chain[Constraint] = self.constraints ++ validation.constraints
    override def encode(b: B): Data = self.encode(g(b))
    override def decode(data: Option[Data.Value]): Validated[Violations, B] =
      self.decode(data).andThen(validation(_).leftMap(Violations.root))

object Dynamic:
  def apply[A](schema: Dynamic[A], description: Option[String]): Dynamic[A] =
    new Dynamic[A](description) { export schema.* }

  val Default: Dynamic[Data.Value] = new Dynamic[Data.Value](None):
    override def constraints: Chain[Constraint] = Chain.empty
    override def isOptional: Boolean = false
    override def encode(a: Data.Value): Data = a
    override def decode(data: Option[Data.Value]): Validated[Violations, Data.Value] =
      Validated.fromOption(data, Violations.rootNec(Violation.required))
