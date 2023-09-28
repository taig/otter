package io.taig.otter

import cats.syntax.all.*
import cats.data.{Chain, Validated}
import io.taig.otter.validation.{Constraint, Validation, Violation, Violations}

sealed abstract class Dictionary[A](description: Option[String]) extends Schema[A](description):
  self =>
  final override type Self[a] = Dictionary[a]

  final override def description(f: Option[String] => Option[String]): Dictionary[A] = Dictionary(this, f(description))

  final override def optional: Dictionary[Option[A]] = new Dictionary[Option[A]](description):
    export self.constraints
    override def isOptional: Boolean = true
    override def decode(data: Option[Data.Object]): Validated[Violations, Option[A]] = self.decode(data).map(_.some)
    override def encodeObject(a: Option[A]): Option[Data.Object] = a.flatMap(self.encodeObject)

  final override def ivalidate[B](validation: Validation[A, B])(g: B => A): Dictionary[B] =
    new Dictionary[B](description):
      export self.isOptional
      override def constraints: Chain[Constraint] = self.constraints ++ validation.constraints
      override def decode(data: Option[Data.Object]): Validated[Violations, B] =
        self.decode(data).andThen(validation(_).leftMap(Violations.root))
      override def encodeObject(b: B): Option[Data.Object] = self.encodeObject(g(b))

  final override def decode(data: Data): Validated[Violations, A] = data match
    case data: Data.Object => decode(Some(data))
    case Data.Null         => decode(None)
    case _                 => Violations.rootNec(Violation.tpe("object", data.name)).invalid
  def decode(data: Option[Data.Object]): Validated[Violations, A]

  final override def encode(a: A): Data = encodeObject(a).getOrElse(Data.Null)
  def encodeObject(a: A): Option[Data.Object]

object Dictionary:
  def apply[A](schema: Dictionary[A], description: Option[String]): Dictionary[A] =
    new Dictionary[A](description) { export schema.* }

  def apply[A, B](key: Schema.Value[A], schema: Schema[B]): Dictionary[Chain[(A, B)]] =
    new Dictionary[Chain[(A, B)]](None):
      override def constraints: Chain[Constraint] = Chain.empty
      override def isOptional: Boolean = false
      override def encodeObject(a: Chain[(A, B)]): Option[Data.Object] =
        Data.Object(a.map { case (a, b) => (key.print(a).orEmpty, schema.encode(b)) }).some
      override def decode(data: Option[Data.Object]): Validated[Violations, Chain[(A, B)]] = data match
        case Some(data) =>
          data.values.traverse { case (a, b) =>
            (key.parse(Some(a).filter(_.nonEmpty)), schema.decode(b)).tupled.leftMap(_.modifyHistory(a /: _))
          }
        case None => Violations.rootNec(Violation.required).invalid
