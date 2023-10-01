package io.taig.otter

import cats.data.{Chain, Validated}
import cats.syntax.all.*
import io.taig.otter.validation.{Constraint, Validation, Violation, Violations}

sealed abstract class Collection[A](description: Option[String]) extends Schema[A](description):
  self =>
  override type Self[a] = Collection.Of[Of, a]
  type Of[a] <: Schema[a]
  type T

  final override def description(f: Option[String] => Option[String]): Collection.Of[Of, A] =
    Collection(this, description)

  final override def optional: Collection.Of[Of, Option[A]] = new Collection[Option[A]](description):
    export self.{constraints, Of, T}
    override def isOptional: Boolean = true
    override def decodeArray(data: Option[Data.Array]): Validated[Violations, Option[A]] =
      data.fold(none.valid)(_ => self.decodeArray(data).map(_.some))
    override def encodeArray(a: Option[A]): Option[Chain[Data]] = a.flatMap(self.encodeArray)
    override def parse(values: Option[Chain[Option[String]]])(using
        Of[T] <:< Schema.Value[T]
    ): Validated[Violations, Option[A]] =
      values.fold(none.valid)(_ => self.parse(values).map(_.some))
    override def print(a: Option[A])(using Of[T] <:< Schema.Value[T]): Option[Chain[Option[String]]] =
      a.flatMap(self.print)

  final override def ivalidate[B](validation: Validation[A, B])(g: B => A): Collection.Of[Of, B] = ???

  final override def decode(data: Option[Data.Value]): Validated[Violations, A] = data match
    case Some(_: Data.Array) => decodeArray(data.asInstanceOf[Option[Data.Array]])
    case Some(data)          => Violations.rootNec(Violation.tpe("array", actual = data.name)).invalid
    case None                => decodeArray(None)
  def decodeArray(data: Option[Data.Array]): Validated[Violations, A]
  final override def encode(a: A): Data = encodeArray(a).map(Data.Array.apply).getOrElse(Data.Null)
  def encodeArray(a: A): Option[Chain[Data]]

  def parse(values: Option[Chain[Option[String]]])(using Of[T] <:< Schema.Value[T]): Validated[Violations, A]
  def print(a: A)(using Of[T] <:< Schema.Value[T]): Option[Chain[Option[String]]]

object Collection:
  type Of[F[a] <: Schema[a], A] = Collection[A] { type Of[a] <: F[a] }

  def apply[F[a] <: Schema[a], A](schema: Collection.Of[F, A], description: Option[String]): Collection.Of[F, A] =
    new Collection[A](description) { export schema.* }

  def apply[F[a] <: Schema[a], A](schema: F[A]): Collection.Of[F, Chain[A]] = new Collection[Chain[A]](None):
    override type Of[a] = F[a]
    override type T = A
    override def constraints: Chain[Constraint] = Chain.empty
    override def isOptional: Boolean = false
    override def decodeArray(data: Option[Data.Array]): Validated[Violations, Chain[A]] = Validated
      .fromOption(data, Violations.rootNec(Violation.required))
      .andThen(_.values.zipWithIndex.traverse { case (data, index) =>
        schema.decode(data).leftMap(_.modifyHistory(index /: _))
      })
    override def encodeArray(a: Chain[A]): Option[Chain[Data]] = a.map(schema.encode).some

    override def parse(
        values: Option[Chain[Option[String]]]
    )(using F[A] <:< Schema.Value[A]): Validated[Violations, Chain[A]] = Validated
      .fromOption(values, Violations.rootNec(Violation.required))
      .andThen(_.zipWithIndex.traverse { case (value, index) =>
        schema.parse(value).leftMap(_.modifyHistory(index /: _))
      })
    override def print(a: Chain[A])(using F[A] <:< Schema.Value[A]): Option[Chain[Option[String]]] =
      a.map(schema.print).some
