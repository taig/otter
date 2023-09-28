package io.taig.otter

import cats.syntax.all.*
import cats.data.{Chain, Validated}
import io.taig.otter.validation.{Constraint, Validation, Violation, Violations}

sealed abstract class Dynamic[A](description: Option[String]) extends Schema[A](description):
  override type Self[a] <: Dynamic[a]

object Dynamic:
  def apply[A](schema: Dynamic[A], description: Option[String]): Dynamic[A] =
    new Dynamic[A](description) { export schema.* }

  abstract private class Root[A](description: Option[String]) extends Dynamic[A](description):
    self =>
    final override type Self[a] = Dynamic[a]
    final override def description(f: Option[String] => Option[String]): Dynamic[A] = Dynamic(this, f(description))
    final override def optional: Dynamic[Option[A]] = new Root[Option[A]](description):
      export self.constraints
      override def isOptional: Boolean = true
      override def encode(a: Option[A]): Data = a.map(self.encode).getOrElse(Data.Null)
      override def decode(data: Data): Validated[Violations, Option[A]] = data match
        case Data.Null => none.valid
        case _         => self.decode(data).map(_.some)
    final override def ivalidate[B](validation: Validation[A, B])(g: B => A): Dynamic[B] = new Root[B](description):
      export self.isOptional
      override def constraints: Chain[Constraint] = self.constraints ++ validation.constraints
      override def encode(b: B): Data = self.encode(g(b))
      override def decode(data: Data): Validated[Violations, B] =
        self.decode(data).andThen(validation(_).leftMap(Violations.root))

  val Default: Dynamic[Data.Value] = new Root[Data.Value](None):
    override def constraints: Chain[Constraint] = Chain.empty
    override def isOptional: Boolean = false
    override def encode(a: Data.Value): Data = a
    override def decode(data: Data): Validated[Violations, Data.Value] = data match
      case data: Data.Value => data.valid
      case Data.Null        => Violations.rootNec(Violation.required).invalid

  sealed abstract class Primitive[A](description: Option[String]) extends Dynamic[A](description) with Schema.Value[A]:
    override type Self[a] = Dynamic.Primitive[a]
    override def description(f: Option[String] => Option[String]): Primitive[A] = Primitive(this, f(description))
    override def optional: Primitive[Option[A]] = ???
    override def ivalidate[B](validation: Validation[A, B])(g: B => A): Primitive[B] = ???

  object Primitive:
    def apply[A](schema: Dynamic.Primitive[A], description: Option[String]): Dynamic.Primitive[A] =
      new Primitive[A](description) { export schema.* }

    val Default: Dynamic.Primitive[Data.Primitive] = new Primitive[Data.Primitive](None):
      override def constraints: Chain[Constraint] = Chain.empty
      override def isOptional: Boolean = false
      override def decode(data: Data): Validated[Violations, Data.Primitive] = ???
      override def encode(a: Data.Primitive): Data = a
      override def parse(value: Option[String]): Validated[Violations, Data.Primitive] = ???
      override def print(a: Data.Primitive): Option[String] = ???
