package io.taig.otter

import cats.data.{Chain, Validated}
import cats.syntax.all.*
import io.taig.otter.validation.{Constraint, Validation, Violation, Violations}

sealed abstract class Dictionary[A](val description: Option[String]) extends Schema[A]:
  self =>
  final override type Self[a] = Dictionary[a]
  final override type Optional[a] = Dictionary[a]

  final override def description(f: Option[String] => Option[String]): Dictionary[A] = Dictionary(this, f(description))

  final override def optional: Dictionary[Option[A]] = new Dictionary[Option[A]](description):
    export self.constraints
    override def isOptional: Boolean = true
    override def decodeObject(data: Option[Data.Object]): Validated[Violations, Option[A]] =
      self.decodeObject(data).map(_.some)
    override def encodeObject(a: Option[A]): Option[Data.Object] = a.flatMap(self.encodeObject)

  final override def ivalidate[B](validation: Validation[A, B])(g: B => A): Dictionary[B] =
    new Dictionary[B](description):
      export self.isOptional
      override def constraints: Chain[Constraint] = self.constraints ++ validation.constraints
      override def decodeObject(data: Option[Data.Object]): Validated[Violations, B] =
        self.decodeObject(data).andThen(validation(_).leftMap(Violations.root))
      override def encodeObject(b: B): Option[Data.Object] = self.encodeObject(g(b))

  final override def decode(data: Option[Data.Value]): Validated[Violations, A] = data match
    case Some(_: Data.Object) => decode(data.asInstanceOf[Option[Data.Object]])
    case Some(data)           => Violations.rootNec(Violation.tpe("object", data.name)).invalid
    case None                 => decode(None)
  def decodeObject(data: Option[Data.Object]): Validated[Violations, A]

  final override def encode(a: A): Data = encodeObject(a).getOrElse(Data.Null)
  def encodeObject(a: A): Option[Data.Object]

object Dictionary:
  def apply[A](schema: Dictionary[A], description: Option[String]): Dictionary[A] =
    new Dictionary[A](description) { export schema.* }

  def apply[A, B](key: Value.Required[A], schema: Schema[B]): Dictionary[Chain[(A, B)]] =
    new Dictionary[Chain[(A, B)]](None):
      override def constraints: Chain[Constraint] = Chain.empty
      override def isOptional: Boolean = false
      override def decodeObject(data: Option[Data.Object]): Validated[Violations, Chain[(A, B)]] = data match
        case Some(data) =>
          data.values.traverse { case (a, b) =>
            (key.parse(a), schema.decode(b)).tupled.leftMap(_.modifyHistory(a /: _))
          }
        case None => Violations.rootNec(Violation.required).invalid
      override def encodeObject(a: Chain[(A, B)]): Option[Data.Object] =
        Data.Object(a.map { case (a, b) => (key.print(a), schema.encode(b)) }).some
