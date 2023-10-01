package io.taig.otter

import cats.data.{Chain, Validated}
import cats.syntax.all.*
import io.taig.otter.validation.{Constraint, Validation, Violation, Violations}

sealed abstract class Collection[A](description: Option[String], val schema: Schema[?]) extends Schema[A](description):
  self =>
  override type Self[a] = Collection.Of[Of, a]
  type Of <: Schema[?]

  final override def description(f: Option[String] => Option[String]): Collection.Of[Of, A] =
    Collection(this, description)

  final override def optional: Collection.Of[Of, Option[A]] = new Collection[Option[A]](description, schema):
    export self.{constraints, Of}
    override def isOptional: Boolean = true
    override def decodeArray(data: Option[Data.Array]): Validated[Violations, Option[A]] =
      data.fold(none.valid)(_ => self.decodeArray(data).map(_.some))
    override def encodeArray(a: Option[A]): Option[Chain[Data]] = a.flatMap(self.encodeArray)
    override def parse(values: Option[Chain[Option[String]]])(using
        Of <:< Schema.Value[?]
    ): Validated[Violations, Option[A]] =
      values.fold(none.valid)(_ => self.parse(values).map(_.some))
    override def print(a: Option[A])(using Of <:< Schema.Value[?]): Option[Chain[Option[String]]] =
      a.flatMap(self.print)

  final override def ivalidate[B](validation: Validation[A, B])(g: B => A): Collection.Of[Of, B] = ???

  final override def decode(data: Option[Data.Value]): Validated[Violations, A] = data match
    case Some(_: Data.Array) => decodeArray(data.asInstanceOf[Option[Data.Array]])
    case Some(data)          => Violations.rootNec(Violation.tpe("array", actual = data.name)).invalid
    case None                => decodeArray(None)
  def decodeArray(data: Option[Data.Array]): Validated[Violations, A]
  final override def encode(a: A): Data = encodeArray(a).map(Data.Array.apply).getOrElse(Data.Null)
  def encodeArray(a: A): Option[Chain[Data]]

  def parse(values: Option[Chain[Option[String]]])(using Of <:< Schema.Value[?]): Validated[Violations, A]
  def print(a: A)(using Of <:< Schema.Value[?]): Option[Chain[Option[String]]]

object Collection:
  type Of[A <: Schema[?], B] = Collection[B] { type Of <: A }

  def apply[A <: Schema[?], B](of: Collection.Of[A, B], description: Option[String]): Collection.Of[A, B] =
    new Collection[B](description, of.schema) { export of.* }

  def apply[F[a] <: Schema[a], A](of: F[A]): Collection.Of[F[A], Chain[A]] = new Collection[Chain[A]](None, of):
    override type Of = F[A]
    override def constraints: Chain[Constraint] = Chain.empty
    override def isOptional: Boolean = false
    override def decodeArray(data: Option[Data.Array]): Validated[Violations, Chain[A]] = Validated
      .fromOption(data, Violations.rootNec(Violation.required))
      .andThen(_.values.zipWithIndex.traverse { case (data, index) =>
        of.decode(data).leftMap(_.modifyHistory(index /: _))
      })
    override def encodeArray(a: Chain[A]): Option[Chain[Data]] = a.map(of.encode).some

    override def parse(
        values: Option[Chain[Option[String]]]
    )(using F[A] <:< Schema.Value[?]): Validated[Violations, Chain[A]] =
      Validated
        .fromOption(values, Violations.rootNec(Violation.required))
        .andThen(_.zipWithIndex.traverse { case (value, index) =>
          of.asInstanceOf[Schema.Value[A]].parse(value).leftMap(_.modifyHistory(index /: _))
        })
    override def print(a: Chain[A])(using F[A] <:< Schema.Value[?]): Option[Chain[Option[String]]] =
      Some(a.map(of.asInstanceOf[Schema.Value[A]].print))
