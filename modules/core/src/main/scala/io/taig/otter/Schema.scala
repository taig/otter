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
  override def self: Annotation[Base.Collection[S, A] | Base.Dictionary[S, A] | Base.Primitive[A]]

object Schema:
  sealed trait Read[+S[a] <: Schema.Read[?, a], +A]:
    def self: Annotation[Base.Collection.Read[S, A] | Base.Dictionary.Read[S, A] | Base.Primitive.Read[A]]

  object Read:
    given [S[a] <: Schema.Read[?, a]]: Functor[Schema.Read[S, *]] with
      override def map[A, B](schema: Schema.Read[S, A])(f: A => B): Schema.Read[S, B] = schema match
        case schema: Schema.Collection.Read[S, A] => schema.map(f)
        case schema: Schema.Dictionary.Read[S, A] => schema.map(f)
        // case schema: Schema.Primitive.Read[A]     => ???

    given [S[a] <: Schema.Read[?, a], A]: Annotated[Schema.Read[S, A]] =
      Annotated[Annotation[Base.Collection.Read[S, A] | Base.Dictionary.Read[S, A] | Base.Primitive.Read[A]]].imap {
        case Annotation(metadata, self: Base.Collection.Read[S, A]) => Collection.Read(Annotation(metadata, self))
        case Annotation(metadata, self: Base.Dictionary.Read[S, A]) => Dictionary.Read(Annotation(metadata, self))
        case Annotation(metadata, self: Base.Primitive.Read[A])     => ???
      }(_.self)

  sealed trait Write[+S[a] <: Schema.Write[?, a], -A]:
    def self: Annotation[Base.Collection.Write[S, A] | Base.Dictionary.Write[S, A] | Base.Primitive.Write[A]]

  object Write:
    def unapply[S[a] <: Schema.Write[?, a], A](
        schema: Schema.Collection.Write[S, A]
    ): Annotation[Base.Collection.Write[S, A]] = schema.self

    given [S[a] <: Schema.Write[?, a]]: Contravariant[Schema.Write[S, *]] with
      override def contramap[A, B](schema: Schema.Write[S, A])(f: B => A): Schema.Write[S, B] = schema match
        case schema: Schema.Collection.Write[S, A] => schema.contramap(f)
        case schema: Schema.Dictionary.Write[S, A] => schema.contramap(f)

    given [S[a] <: Schema.Write[?, a], A]: Annotated[Schema.Write[S, A]] =
      Annotated[Annotation[Base.Collection.Write[S, A] | Base.Dictionary.Write[S, A] | Base.Primitive.Write[A]]].imap {
        case Annotation(metadata, self: Base.Collection.Write[S, A]) => Collection.Write(Annotation(metadata, self))
        case Annotation(metadata, self: Base.Dictionary.Write[S, A]) => Dictionary.Write(Annotation(metadata, self))
        case Annotation(metadata, self: Base.Primitive.Write[A])     => ???
      }(_.self)

  sealed abstract class Collection[+S[a] <: Schema[?, a], A]
      extends Schema[S, A],
        Schema.Collection.Read[S, A],
        Schema.Collection.Write[S, A]:
    override def self: Annotation[Base.Collection[S, A]]

  object Collection:
    sealed trait Read[+S[a] <: Schema.Read[?, a], +A] extends Schema.Read[S, A]:
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

      given [S[a] <: Schema.Read[?, a], A]: Annotated[Schema.Collection.Read[S, A]] =
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

      given [S[a] <: Schema.Write[?, a], A]: Annotated[Schema.Collection.Write[S, A]] =
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

  sealed abstract class Dictionary[+S[a] <: Schema[?, a], A]
      extends Schema[S, A],
        Schema.Dictionary.Read[S, A],
        Schema.Dictionary.Write[S, A]:
    override def self: Annotation[Base.Dictionary[S, A]]

  object Dictionary:
    sealed trait Read[+S[a] <: Schema.Read[?, a], +A] extends Schema.Read[S, A]:
      override def self: Annotation[Base.Dictionary.Read[S, A]]

    object Read:
      def apply[S[a] <: Schema.Read[?, a], A](
          annotation: Annotation[Base.Dictionary.Read[S, A]]
      ): Schema.Dictionary.Read[S, A] = new Read[S, A]:
        override def self: Annotation[Base.Dictionary.Read[S, A]] = annotation

      def unapply[S[a] <: Schema.Read[?, a], A](
          schema: Schema.Dictionary.Read[S, A]
      ): Annotation[Base.Dictionary.Read[S, A]] = schema.self

      given [S[a] <: Schema.Read[?, a]]: Functor[Schema.Dictionary.Read[S, *]] =
        Functor[[a] =>> Annotation[Base.Dictionary.Read[S, a]]].imapK([A] =>
          (annotation: Annotation[Base.Dictionary.Read[S, A]]) => Read(annotation)
        )([A] => (schema: Schema.Dictionary.Read[S, A]) => schema.self)

      given [S[a] <: Schema.Read[?, a], A]: Annotated[Schema.Dictionary.Read[S, A]] =
        Annotated[Annotation[Base.Dictionary.Read[S, A]]].imap(Read.apply)(_.self)

      given Self.Dictionary.Read[Schema.Dictionary.Read, Schema.Read[?, *]] = Self.Dictionary
        .Read[[s[a] <: Schema.Read[?, a], a] =>> Annotation[Base.Dictionary.Read[s, a]], Schema.Read[?, *]]
        .imapK([s[a] <: Schema.Read[?, a], a] => (self: Annotation[Base.Dictionary.Read[s, a]]) => Read(self))(
          [s[a] <: Schema.Read[?, a], a] => (schema: Schema.Dictionary.Read[s, a]) => schema.self
        )

    sealed trait Write[+S[a] <: Schema.Write[?, a], -A] extends Schema.Write[S, A]:
      override def self: Annotation[Base.Dictionary.Write[S, A]]

    object Write:
      def apply[S[a] <: Schema.Write[?, a], A](
          annotation: Annotation[Base.Dictionary.Write[S, A]]
      ): Schema.Dictionary.Write[S, A] = new Write[S, A]:
        override def self: Annotation[Base.Dictionary.Write[S, A]] = annotation

      def unapply[S[a] <: Schema.Write[?, a], A](
          schema: Schema.Dictionary.Write[S, A]
      ): Annotation[Base.Dictionary.Write[S, A]] = schema.self

      given [S[a] <: Schema.Write[?, a]]: Contravariant[Schema.Dictionary.Write[S, *]] =
        Contravariant[[a] =>> Annotation[Base.Dictionary.Write[S, a]]].imapK([A] =>
          (annotation: Annotation[Base.Dictionary.Write[S, A]]) => Write(annotation)
        )([A] => (schema: Schema.Dictionary.Write[S, A]) => schema.self)

      given [S[a] <: Schema.Write[?, a], A]: Annotated[Schema.Dictionary.Write[S, A]] =
        Annotated[Annotation[Base.Dictionary.Write[S, A]]].imap(Write.apply)(_.self)

      given Self.Dictionary.Write[Schema.Dictionary.Write, Schema.Write[?, *]] = Self.Dictionary
        .Write[[s[a] <: Schema.Write[?, a], a] =>> Annotation[Base.Dictionary.Write[s, a]], Schema.Write[?, *]]
        .imapK([s[a] <: Schema.Write[?, a], a] => (self: Annotation[Base.Dictionary.Write[s, a]]) => Write(self))(
          [s[a] <: Schema.Write[?, a], a] => (schema: Schema.Dictionary.Write[s, a]) => schema.self
        )

    def apply[S[a] <: Schema[?, a], A](annotation: Annotation[Base.Dictionary[S, A]]): Schema.Dictionary[S, A] =
      new Dictionary[S, A]:
        override def self: Annotation[Base.Dictionary[S, A]] = annotation

    def unapply[S[a] <: Schema[?, a], A](schema: Schema.Dictionary[S, A]): Annotation[Base.Dictionary[S, A]] =
      schema.self

    given [S[a] <: Schema[?, a]]: Invariant[Schema.Dictionary[S, *]] =
      Invariant[[a] =>> Annotation[Base.Dictionary[S, a]]].imapK([A] =>
        (annotation: Annotation[Base.Dictionary[S, A]]) => Dictionary(annotation)
      )([A] => (schema: Schema.Dictionary[S, A]) => schema.self)

    given [S[a] <: Schema[?, a], A]: Annotated[Schema.Dictionary[S, A]] =
      Annotated[Annotation[Base.Dictionary[S, A]]].imap(Dictionary.apply)(_.self)

    given Self.Dictionary[Schema.Dictionary, Schema[?, *]] = Self
      .Dictionary[[s[a] <: Schema[?, a], a] =>> Annotation[Base.Dictionary[s, a]], Schema[?, *]]
      .imapK([s[a] <: Schema[?, a], a] => (self: Annotation[Base.Dictionary[s, a]]) => Dictionary(self))(
        [s[a] <: Schema[?, a], a] => (schema: Schema.Dictionary[s, a]) => schema.self
      )

  sealed abstract class Primitive[A] extends Schema[Nothing, A], Schema.Primitive.Read[A], Schema.Primitive.Write[A]:
    override def self: Annotation[Base.Primitive[A]]

  object Primitive:
    sealed trait Read[+A] extends Schema.Read[Nothing, A]:
      override def self: Annotation[Base.Primitive.Read[A]]

    sealed trait Write[-A] extends Schema.Write[Nothing, A]:
      override def self: Annotation[Base.Primitive.Write[A]]

    sealed abstract class Boolean[A]
        extends Schema.Primitive[A],
          Schema.Primitive.Boolean.Read[A],
          Schema.Primitive.Boolean.Write[A]:
      override def self: Annotation[Base.Primitive.Boolean[A]]

    object Boolean:
      sealed trait Read[+A] extends Schema.Primitive.Read[A]:
        override def self: Annotation[Base.Primitive.Boolean.Read[A]]

      sealed trait Write[-A] extends Schema.Primitive.Write[A]:
        override def self: Annotation[Base.Primitive.Boolean.Write[A]]

      object Write

      def apply[A](annotation: Annotation[Base.Primitive.Boolean[A]]): Schema.Primitive.Boolean[A] = new Boolean[A]:
        override def self: Annotation[Base.Primitive.Boolean[A]] = annotation

      def unapply[A](schema: Schema.Primitive.Boolean[A]): Annotation[Base.Primitive.Boolean[A]] = schema.self

      given Invariant[Schema.Primitive.Boolean] = Invariant[[a] =>> Annotation[Base.Primitive.Boolean[a]]].imapK([A] =>
        (annotation: Annotation[Base.Primitive.Boolean[A]]) => Boolean(annotation)
      )([A] => (schema: Schema.Primitive.Boolean[A]) => schema.self)

      given [A]: Annotated[Schema.Primitive.Boolean[A]] =
        Annotated[Annotation[Base.Primitive.Boolean[A]]].imap(Boolean.apply)(_.self)

      given Self.Primitive.Boolean[Schema.Primitive.Boolean] = Self.Primitive
        .Boolean[[a] =>> Annotation[Base.Primitive.Boolean[a]]]
        .imapK([a] => (self: Annotation[Base.Primitive.Boolean[a]]) => Boolean(self))([a] =>
          (schema: Schema.Primitive.Boolean[a]) => schema.self
        )

    sealed abstract class Number[A]
        extends Schema.Primitive[A],
          Schema.Primitive.Number.Read[A],
          Schema.Primitive.Number.Write[A]:
      override def self: Annotation[Base.Primitive.Number[A]]

    object Number:
      sealed trait Read[+A] extends Schema.Primitive.Read[A]:
        override def self: Annotation[Base.Primitive.Number.Read[A]]

      object Read:
        def apply[A](annotation: Annotation[Base.Primitive.Number.Read[A]]): Schema.Primitive.Number.Read[A] =
          new Schema.Primitive.Number.Read[A]:
            override def self: Annotation[Base.Primitive.Number.Read[A]] = annotation

        def unapply[A](schema: Schema.Primitive.Number.Read[A]): Annotation[Base.Primitive.Number.Read[A]] =
          schema.self

        given Functor[Schema.Primitive.Number.Read] = Functor[[a] =>> Annotation[Base.Primitive.Number.Read[a]]].imapK(
          [A] => (annotation: Annotation[Base.Primitive.Number.Read[A]]) => Read(annotation)
        )([A] => (schema: Schema.Primitive.Number.Read[A]) => schema.self)

        given [A]: Annotated[Schema.Primitive.Number.Read[A]] =
          Annotated[Annotation[Base.Primitive.Number.Read[A]]].imap(Read.apply)(_.self)

      sealed trait Write[-A] extends Schema.Primitive.Write[A]:
        override def self: Annotation[Base.Primitive.Number.Write[A]]

      object Write:
        def apply[A](annotation: Annotation[Base.Primitive.Number.Write[A]]): Schema.Primitive.Number.Write[A] =
          new Schema.Primitive.Number.Write[A]:
            override def self: Annotation[Base.Primitive.Number.Write[A]] = annotation

        def unapply[A](schema: Schema.Primitive.Number.Write[A]): Annotation[Base.Primitive.Number.Write[A]] =
          schema.self

        given Contravariant[Schema.Primitive.Number.Write] =
          Contravariant[[a] =>> Annotation[Base.Primitive.Number.Write[a]]].imapK([A] =>
            (annotation: Annotation[Base.Primitive.Number.Write[A]]) => Write(annotation)
          )([A] => (schema: Schema.Primitive.Number.Write[A]) => schema.self)

        given [A]: Annotated[Schema.Primitive.Number.Write[A]] =
          Annotated[Annotation[Base.Primitive.Number.Write[A]]].imap(Write.apply)(_.self)

    sealed abstract class Text[A]
        extends Schema.Primitive[A],
          Schema.Primitive.Text.Read[A],
          Schema.Primitive.Text.Write[A]:
      override def self: Annotation[Base.Primitive.Text[A]]

    object Text:
      sealed trait Read[+A] extends Schema.Primitive.Read[A]:
        override def self: Annotation[Base.Primitive.Text.Read[A]]

      object Read:
        def apply[A](annotation: Annotation[Base.Primitive.Text.Read[A]]): Schema.Primitive.Text.Read[A] =
          new Schema.Primitive.Text.Read[A]:
            override def self: Annotation[Base.Primitive.Text.Read[A]] = annotation

        def unapply[A](schema: Schema.Primitive.Text.Read[A]): Annotation[Base.Primitive.Text.Read[A]] = schema.self

        given Functor[Schema.Primitive.Text.Read] = Functor[[a] =>> Annotation[Base.Primitive.Text.Read[a]]].imapK(
          [A] => (annotation: Annotation[Base.Primitive.Text.Read[A]]) => Read(annotation)
        )([A] => (schema: Schema.Primitive.Text.Read[A]) => schema.self)

        given [A]: Annotated[Schema.Primitive.Text.Read[A]] =
          Annotated[Annotation[Base.Primitive.Text.Read[A]]].imap(Read.apply)(_.self)

      sealed trait Write[-A] extends Schema.Primitive.Write[A]:
        override def self: Annotation[Base.Primitive.Text.Write[A]]

      object Write:
        def apply[A](annotation: Annotation[Base.Primitive.Text.Write[A]]): Schema.Primitive.Text.Write[A] =
          new Schema.Primitive.Text.Write[A]:
            override def self: Annotation[Base.Primitive.Text.Write[A]] = annotation

        def unapply[A](schema: Schema.Primitive.Text.Write[A]): Annotation[Base.Primitive.Text.Write[A]] = schema.self

        given Contravariant[Schema.Primitive.Text.Write] =
          Contravariant[[a] =>> Annotation[Base.Primitive.Text.Write[a]]].imapK([A] =>
            (annotation: Annotation[Base.Primitive.Text.Write[A]]) => Write(annotation)
          )([A] => (schema: Schema.Primitive.Text.Write[A]) => schema.self)

        given [A]: Annotated[Schema.Primitive.Text.Write[A]] =
          Annotated[Annotation[Base.Primitive.Text.Write[A]]].imap(Write.apply)(_.self)

    given Invariant[Schema.Primitive] = ???

  def unapply[S[a] <: Schema[?, a], A](
      schema: Schema[S, A]
  ): Annotation[Base.Collection[S, A] | Base.Dictionary[S, A] | Base.Primitive[A]] = schema.self

  given [S[a] <: Schema[?, a]]: Invariant[Schema[S, *]] with
    override def imap[A, B](fa: Schema[S, A])(f: A => B)(g: B => A): Schema[S, B] = fa match
      case schema: Schema.Collection[S, A] => schema.imap(f)(g)
      case schema: Schema.Dictionary[S, A] => schema.imap(f)(g)
      case schema: Schema.Primitive[A]     => schema.imap(f)(g)

  given [S[a] <: Schema[?, a], A]: Annotated[Schema[S, A]] =
    Annotated[Annotation[Base.Collection[S, A] | Base.Dictionary[S, A] | Base.Primitive[A]]].imap {
      case Annotation(metadata, self: Base.Collection[S, A]) => Collection(Annotation(metadata, self))
      case Annotation(metadata, self: Base.Dictionary[S, A]) => Dictionary(Annotation(metadata, self))
      case Annotation(metadata, self: Base.Primitive[A])     => ???
    }(_.self)
