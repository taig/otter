package io.taig.otter

import cats.Contravariant
import cats.Functor
import cats.Invariant
import cats.syntax.all.*
import io.taig.otter as Self
import io.taig.otter.base as Base
import io.taig.otter.base.Collection
import io.taig.otter.syntax.CatsSyntax.*

sealed abstract class Schema[+S[a] <: Schema[?, a], A] extends Schema.Read[S, A], Schema.Write[S, A]:
  override def self: Annotation[Base.Collection[S, A]]

object Schema:
  sealed trait Read[+S[a] <: Schema.Read[?, a], A]:
    def self: Annotation[Base.Collection.Read[S, A]]

  object Read:
    given [S[a] <: Schema.Read[?, a]]: Functor[Schema.Read[S, *]] with
      override def map[A, B](schema: Schema.Read[S, A])(f: A => B): Schema.Read[S, B] = schema match
        case schema: Schema.Collection.Read[S, A] => schema.map(f)

    given [S[a] <: Schema.Read[?, a], A]: Annotated[Schema.Read[S, A]] =
      Annotated[Annotation[Base.Collection.Read[S, A]]].imap {
        case annotation: Annotation[Base.Collection.Read[S, A]] => Collection.Read(annotation)
      }(_.self)

  sealed trait Write[+S[a] <: Schema.Write[?, a], -A]:
    def self: Annotation[Base.Collection.Write[S, A]]

  object Write:
    def unapply[S[a] <: Schema.Write[?, a], A](
        schema: Schema.Collection.Write[S, A]
    ): Annotation[Base.Collection.Write[S, A]] = schema.self

    given [S[a] <: Schema.Write[?, a]]: Contravariant[Schema.Write[S, *]] with
      override def contramap[A, B](schema: Schema.Write[S, A])(f: B => A): Schema.Write[S, B] = schema match
        case schema: Schema.Collection.Write[S, A] => schema.contramap(f)

    given [S[a] <: Schema.Write[?, a], A]: Annotated[Schema.Write[S, A]] =
      Annotated[Annotation[Base.Collection.Write[S, A]]].imap {
        case annotation: Annotation[Base.Collection.Write[S, A]] => Collection.Write(annotation)
      }(_.self)

  sealed abstract class Collection[+S[a] <: Schema[?, a], A]
      extends Schema[S, A],
        Collection.Read[S, A],
        Collection.Write[S, A]:
    override def self: Annotation[Base.Collection[S, A]]

  object Collection:
    sealed trait Read[+S[a] <: Schema.Read[?, a], A] extends Schema.Read[S, A]:
      override def self: Annotation[Base.Collection.Read[S, A]]

    object Read:
      def apply[S[a] <: Schema.Read[?, a], A](
          annotation: Annotation[Base.Collection.Read[S, A]]
      ): Schema.Collection.Read[S, A] = new Read[S, A]:
        override def self: Annotation[Base.Collection.Read[S, A]] = annotation

      def unapply[S[a] <: Schema.Read[?, a], A](
          schema: Schema.Collection.Read[S, A]
      ): Annotation[Base.Collection.Read[S, A]] = schema.self

      given [S[a] <: Schema.Read[?, a]]: Functor[Schema.Collection.Read[S, *]] =
        Functor[[a] =>> Annotation[Base.Collection.Read[S, a]]].imapK([A] =>
          (annotation: Annotation[Base.Collection.Read[S, A]]) => Read(annotation)
        )([A] => (schema: Schema.Collection.Read[S, A]) => schema.self)

      given [S[a] <: Schema.Read[?, a], A]: Annotated[Schema.Read[S, A]] =
        Annotated[Annotation[Base.Collection.Read[S, A]]].imap(Read.apply)(_.self)

      given Self.Collection.Read[Schema.Collection.Read, Schema.Read[?, *]] = Self.Collection
        .Read[[s[a] <: Schema.Read[?, a], a] =>> Annotation[Base.Collection.Read[s, a]], Schema.Read[?, *]]
        .imapK([s[a] <: Schema.Read[?, a], a] => (self: Annotation[Base.Collection.Read[s, a]]) => Read(self))(
          [s[a] <: Schema.Read[?, a], a] => (schema: Schema.Collection.Read[s, a]) => schema.self
        )

    sealed trait Write[+S[a] <: Schema.Write[?, a], -A] extends Schema.Write[S, A]:
      override def self: Annotation[Base.Collection.Write[S, A]]

    object Write:
      def apply[S[a] <: Schema.Write[?, a], A](
          annotation: Annotation[Base.Collection.Write[S, A]]
      ): Schema.Collection.Write[S, A] = new Write[S, A]:
        override def self: Annotation[Base.Collection.Write[S, A]] = annotation

      def unapply[S[a] <: Schema.Write[?, a], A](
          schema: Schema.Collection.Write[S, A]
      ): Annotation[Base.Collection.Write[S, A]] = schema.self

      given [S[a] <: Schema.Write[?, a]]: Contravariant[Schema.Collection.Write[S, *]] =
        Contravariant[[a] =>> Annotation[Base.Collection.Write[S, a]]].imapK([A] =>
          (annotation: Annotation[Base.Collection.Write[S, A]]) => Write(annotation)
        )([A] => (schema: Schema.Collection.Write[S, A]) => schema.self)

      given [S[a] <: Schema.Write[?, a], A]: Annotated[Schema.Write[S, A]] =
        Annotated[Annotation[Base.Collection.Write[S, A]]].imap(Write.apply)(_.self)

      given Self.Collection.Write[Schema.Collection.Write, Schema.Write[?, *]] = Self.Collection
        .Write[[s[a] <: Schema.Write[?, a], a] =>> Annotation[Base.Collection.Write[s, a]], Schema.Write[?, *]]
        .imapK([s[a] <: Schema.Write[?, a], a] => (self: Annotation[Base.Collection.Write[s, a]]) => Write(self))(
          [s[a] <: Schema.Write[?, a], a] => (schema: Schema.Collection.Write[s, a]) => schema.self
        )

    def apply[S[a] <: Schema[?, a], A](annotation: Annotation[Base.Collection[S, A]]): Schema.Collection[S, A] =
      new Collection[S, A]:
        override def self: Annotation[Base.Collection[S, A]] = annotation

    def unapply[S[a] <: Schema[?, a], A](schema: Schema.Collection[S, A]): Annotation[Base.Collection[S, A]] =
      schema.self

    given [S[a] <: Schema[?, a]]: Invariant[Schema.Collection[S, *]] =
      Invariant[[a] =>> Annotation[Base.Collection[S, a]]].imapK([A] =>
        (annotation: Annotation[Base.Collection[S, A]]) => Collection(annotation)
      )([A] => (schema: Schema.Collection[S, A]) => schema.self)

    given [S[a] <: Schema[?, a], A]: Annotated[Schema.Collection[S, A]] =
      Annotated[Annotation[Base.Collection[S, A]]].imap(Collection.apply)(_.self)

    given Self.Collection[Schema.Collection, Schema[?, *]] = Self
      .Collection[[s[a] <: Schema[?, a], a] =>> Annotation[Base.Collection[s, a]], Schema[?, *]]
      .imapK([s[a] <: Schema[?, a], a] => (self: Annotation[Base.Collection[s, a]]) => Collection(self))(
        [s[a] <: Schema[?, a], a] => (schema: Schema.Collection[s, a]) => schema.self
      )

  def unapply[S[a] <: Schema[?, a], A](schema: Schema[S, A]): Annotation[Base.Collection[S, A]] =
    schema.self

  given [S[a] <: Schema[?, a]]: Invariant[Schema[S, *]] with
    override def imap[A, B](fa: Schema[S, A])(f: A => B)(g: B => A): Schema[S, B] = fa match
      case schema: Schema.Collection[S, A] => schema.imap(f)(g)

  given [S[a] <: Schema[?, a], A]: Annotated[Schema[S, A]] =
    Annotated[Annotation[Base.Collection[S, A]]].imap { case annotation: Annotation[Base.Collection[S, A]] =>
      Collection(annotation)
    }(_.self)
