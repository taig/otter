package io.taig.otter

import cats.data.{Chain, Validated}
import cats.syntax.all.*
import io.taig.otter.validation.{Constraint, Validation, Violation, Violations}

sealed abstract class Collection[A](description: Option[String]) extends Schema[A](description):
  self =>
  override type Self[a] = Collection.Of[Of, a]
  type Of[a] <: Schema[a]

  final override def description(f: Option[String] => Option[String]): Collection.Of[Of, A] =
    Collection(this, f(description))

  final override def optional: Collection.Of[Of, Option[A]] = new Collection[Option[A]](description):
    export self.{constraints, Of}
    override def isOptional: Boolean = true
    override def decode(data: Option[Data.Array]): Validated[Violations, Option[A]] =
      data.fold(none.valid)(_ => self.decode(data).map(_.some))
    override def encodeArray(a: Option[A]): Option[Data.Array] = a.flatMap(self.encodeArray)

  final override def ivalidate[B](validation: Validation[A, B])(g: B => A): Collection.Of[Of, B] =
    new Collection[B](description):
      export self.{isOptional, Of}
      override def constraints: Chain[Constraint] = self.constraints ++ validation.constraints
      override def decode(data: Option[Data.Array]): Validated[Violations, B] =
        self.decode(data).andThen(validation(_).leftMap(Violations.root))
      override def encodeArray(b: B): Option[Data.Array] = self.encodeArray(g(b))

  final override def decode(data: Data): Validated[Violations, A] = data match
    case data: Data.Array => decode(Some(data))
    case Data.Null        => decode(None)
    case _                => Violations.rootNec(Violation.tpe("array", actual = data.name)).invalid
  def decode(data: Option[Data.Array]): Validated[Violations, A]
  final override def encode(a: A): Data = encodeArray(a).getOrElse(Data.Null)
  def encodeArray(a: A): Option[Data.Array]

object Collection:
  type Of[F[a] <: Schema[a], A] = Collection[A] { type Of[a] = F[a] }

  def apply[F[a] <: Schema[a], A](schema: Collection.Of[F, A], description: Option[String]): Collection.Of[F, A] =
    new Collection[A](description) { export schema.* }

  def apply[F[a] <: Schema[a], A](schema: F[A]): Collection.Of[F, Chain[A]] = new Collection[Chain[A]](None):
    override type Of[a] = F[a]
    override def constraints: Chain[Constraint] = Chain.empty
    override def isOptional: Boolean = false
    override def decode(data: Option[Data.Array]): Validated[Violations, Chain[A]] = data match
      case Some(data) =>
        data.values.zipWithIndex.traverse { case (data, index) =>
          schema.decode(data).leftMap(_.modifyHistory(index /: _))
        }
      case None => Violations.rootNec(Violation.required).invalid
    override def encodeArray(a: Chain[A]): Option[Data.Array] = Data.Array(a.map(schema.encode)).some
