package io.taig.otter

import io.taig.otter as Self
import io.taig.otter.base as Base
import io.taig.otter.syntax.CatsSyntax.*
import cats.Contravariant
import cats.Invariant
import cats.Functor

sealed abstract class Schema[+S[a] <: Schema[?, a], A] extends Schema.Read[S, A], Schema.Write[S, A]:
  override def self: Annotation[Schema.Of[S, A]]

object Schema:
  type Of[+S[_], A] = Base.Coerce[S, A] | Base.Collection[S, A] | Base.Constant[S, A] | Base.Dictionary[S, A] |
    Base.Enumeration[S, A] | Base.Nullish[S, A] | Base.Primitive[A] | Base.Record[S, A] | Base.Tuple[S, A] |
    Base.Union[S, A]

  sealed trait Read[+S[a] <: Schema.Read[?, a], +A]:
    def self: Annotation[Schema.Read.Of[S, A]]

  object Read:
    type Of[+S[_], +A] = Base.Coerce.Read[S, A] | Base.Collection.Read[S, A] | Base.Constant.Read[S, A] |
      Base.Dictionary.Read[S, A] | Base.Enumeration.Read[S, A] | Base.Nullish.Read[S, A] | Base.Primitive.Read[A] |
      Base.Record.Read[S, A] | Base.Tuple.Read[S, A] | Base.Union.Read[S, A]

    def apply[S[a] <: Schema.Read[?, a], A](annotation: Annotation[Schema.Read.Of[S, A]]): Schema.Read[S, A] =
      annotation.self match
        case self: Base.Coerce.Read[S, A]      => Schema.Coerce.Read(annotation.copy(self = self))
        case self: Base.Collection.Read[S, A]  => Schema.Collection.Read(annotation.copy(self = self))
        case self: Base.Constant.Read[S, A]    => Schema.Constant.Read(annotation.copy(self = self))
        case self: Base.Dictionary.Read[S, A]  => Schema.Dictionary.Read(annotation.copy(self = self))
        case self: Base.Enumeration.Read[S, A] => Schema.Enumeration.Read(annotation.copy(self = self))
        case self: Base.Nullish.Read[S, A]     => Schema.Nullish.Read(annotation.copy(self = self))
        case self: Base.Primitive.Read[A]      => Schema.Primitive.Read(annotation.copy(self = self))
        case self: Base.Record.Read[S, A]      => Schema.Record.Read(annotation.copy(self = self))
        case self: Base.Tuple.Read[S, A]       => Schema.Tuple.Read(annotation.copy(self = self))
        case self: Base.Union.Read[S, A]       => Schema.Union.Read(annotation.copy(self = self))

    def unapply[S[a] <: Schema.Read[?, a], A](schema: Schema.Read[S, A]): Annotation[Schema.Read.Of[S, A]] = schema.self

    given [S[a] <: Schema.Read[?, a], A]: Annotated[Schema.Read[S, A]] =
      Annotated[Annotation[Schema.Read.Of[S, A]]].imap(Schema.Read.apply)(_.self)

    given Coerceable[Schema.Coerce.Read, Schema.Read[?, *]] with
      override val coerce: Self.Coerce.Read[Schema.Coerce.Read, Schema.Read[?, *]] =
        Self.Coerce.Read[Schema.Coerce.Read, Schema.Read[?, *]]

    given Nullable[Schema.Nullish.Read, Schema.Read[?, *]] with
      override val nullish: Self.Nullish.Read[Schema.Nullish.Read, Schema.Read[?, *]] =
        Self.Nullish.Read[Schema.Nullish.Read, Schema.Read[?, *]]

    given Tupleable[Schema.Tuple.Read, Schema.Read[?, *]] with
      override val tuple: Self.Tuple[Schema.Tuple.Read, Schema.Read[?, *]] =
        Self.Tuple[Schema.Tuple.Read, Schema.Read[?, *]]

  sealed trait Write[+S[a] <: Schema.Write[?, a], -A]:
    def self: Annotation[Schema.Write.Of[S, A]]

  object Write:
    type Of[+S[_], -A] = Base.Coerce.Write[S, A] | Base.Collection.Write[S, A] | Base.Constant.Write[S, A] |
      Base.Dictionary.Write[S, A] | Base.Enumeration.Write[S, A] | Base.Nullish.Write[S, A] | Base.Primitive.Write[A] |
      Base.Record.Write[S, A] | Base.Tuple.Write[S, A] | Base.Union.Write[S, A]

    def apply[S[a] <: Schema.Write[?, a], A](annotation: Annotation[Schema.Write.Of[S, A]]): Schema.Write[S, A] =
      annotation.self match
        case self: Base.Coerce.Write[S, A]      => Schema.Coerce.Write(annotation.copy(self = self))
        case self: Base.Collection.Write[S, A]  => Schema.Collection.Write(annotation.copy(self = self))
        case self: Base.Constant.Write[S, A]    => Schema.Constant.Write(annotation.copy(self = self))
        case self: Base.Dictionary.Write[S, A]  => Schema.Dictionary.Write(annotation.copy(self = self))
        case self: Base.Enumeration.Write[S, A] => Schema.Enumeration.Write(annotation.copy(self = self))
        case self: Base.Nullish.Write[S, A]     => Schema.Nullish.Write(annotation.copy(self = self))
        case self: Base.Primitive.Write[A]      => Schema.Primitive.Write(annotation.copy(self = self))
        case self: Base.Record.Write[S, A]      => Schema.Record.Write(annotation.copy(self = self))
        case self: Base.Tuple.Write[S, A]       => Schema.Tuple.Write(annotation.copy(self = self))
        case self: Base.Union.Write[S, A]       => Schema.Union.Write(annotation.copy(self = self))

    def unapply[S[a] <: Schema.Write[?, a], A](schema: Schema.Write[S, A]): Annotation[Schema.Write.Of[S, A]] =
      schema.self

    given [S[a] <: Schema.Write[?, a], A]: Annotated[Schema.Write[S, A]] =
      Annotated[Annotation[Schema.Write.Of[S, A]]].imap(Schema.Write.apply)(_.self)

    given Coerceable[Schema.Coerce.Write, Schema.Write[?, *]] with
      override val coerce: Self.Coerce.Write[Schema.Coerce.Write, Schema.Write[?, *]] =
        Self.Coerce.Write[Schema.Coerce.Write, Schema.Write[?, *]]

    given Nullable[Schema.Nullish.Write, Schema.Write[?, *]] with
      override val nullish: Self.Nullish.Write[Schema.Nullish.Write, Schema.Write[?, *]] =
        Self.Nullish.Write[Schema.Nullish.Write, Schema.Write[?, *]]

    given Tupleable[Schema.Tuple.Write, Schema.Write[?, *]] with
      override val tuple: Self.Tuple[Schema.Tuple.Write, Schema.Write[?, *]] =
        Self.Tuple[Schema.Tuple.Write, Schema.Write[?, *]]

  sealed abstract class Coerce[+S[a] <: Schema[?, a], A]
      extends Schema[S, A],
        Schema.Coerce.Read[S, A],
        Schema.Coerce.Write[S, A]:
    override def self: Annotation[Base.Coerce[S, A]]

  object Coerce:
    sealed trait Read[+S[a] <: Schema.Read[?, a], +A] extends Schema.Read[S, A]:
      override def self: Annotation[Base.Coerce.Read[S, A]]

    object Read:
      def apply[S[a] <: Schema.Read[?, a], A](
          annotation: Annotation[Base.Coerce.Read[S, A]]
      ): Schema.Coerce.Read[S, A] = new Read[S, A]:
        override def self: Annotation[Base.Coerce.Read[S, A]] = annotation

      def unapply[S[a] <: Schema.Read[?, a], A](schema: Schema.Coerce.Read[S, A]): Annotation[Base.Coerce.Read[S, A]] =
        schema.self

      given [S[a] <: Schema.Read[?, a]]: Functor[Schema.Coerce.Read[S, *]] =
        Functor[[a] =>> Annotation[Base.Coerce.Read[S, a]]].imapK([A] =>
          (annotation: Annotation[Base.Coerce.Read[S, A]]) => Schema.Coerce.Read(annotation)
        )([A] => (schema: Schema.Coerce.Read[S, A]) => schema.self)

      given [S[a] <: Schema.Read[?, a], A]: Annotated[Schema.Coerce.Read[S, A]] =
        Annotated[Annotation[Base.Coerce.Read[S, A]]].imap(Schema.Coerce.Read.apply)(_.self)

      given Self.Coerce.Read[Schema.Coerce.Read, Schema.Read[?, *]] = Self.Coerce
        .Read[[s[a] <: Schema.Read[?, a], a] =>> Annotation[Base.Coerce.Read[s, a]], Schema.Read[?, *]]
        .imapK([S[a] <: Schema.Read[?, a], A] => (annotation: Annotation[Base.Coerce.Read[S, A]]) => Read(annotation))(
          [S[a] <: Schema.Read[?, a], A] => (schema: Schema.Coerce.Read[S, A]) => schema.self
        )

    sealed trait Write[+S[a] <: Schema.Write[?, a], -A] extends Schema.Write[S, A]:
      override def self: Annotation[Base.Coerce.Write[S, A]]

    object Write:
      def apply[S[a] <: Schema.Write[?, a], A](
          annotation: Annotation[Base.Coerce.Write[S, A]]
      ): Schema.Coerce.Write[S, A] = new Write[S, A]:
        override def self: Annotation[Base.Coerce.Write[S, A]] = annotation

      def unapply[S[a] <: Schema.Write[?, a], A](
          schema: Schema.Coerce.Write[S, A]
      ): Annotation[Base.Coerce.Write[S, A]] = schema.self

      given [S[a] <: Schema.Write[?, a]]: Contravariant[Schema.Coerce.Write[S, *]] =
        Contravariant[[a] =>> Annotation[Base.Coerce.Write[S, a]]].imapK([A] =>
          (annotation: Annotation[Base.Coerce.Write[S, A]]) => Schema.Coerce.Write(annotation)
        )([A] => (schema: Schema.Coerce.Write[S, A]) => schema.self)

      given [S[a] <: Schema.Write[?, a], A]: Annotated[Schema.Coerce.Write[S, A]] =
        Annotated[Annotation[Base.Coerce.Write[S, A]]].imap(Schema.Coerce.Write.apply)(_.self)

      given Self.Coerce.Write[Schema.Coerce.Write, Schema.Write[?, *]] = Self.Coerce
        .Write[[s[a] <: Schema.Write[?, a], a] =>> Annotation[Base.Coerce.Write[s, a]], Schema.Write[?, *]]
        .imapK([S[a] <: Schema.Write[?, a], A] =>
          (annotation: Annotation[Base.Coerce.Write[S, A]]) => Write(annotation)
        )([S[a] <: Schema.Write[?, a], A] => (schema: Schema.Coerce.Write[S, A]) => schema.self)

    def apply[S[a] <: Schema[?, a], A](annotation: Annotation[Base.Coerce[S, A]]): Schema.Coerce[S, A] =
      new Coerce[S, A]:
        override def self: Annotation[Base.Coerce[S, A]] = annotation

    def unapply[S[a] <: Schema[?, a], A](schema: Schema.Coerce[S, A]): Annotation[Base.Coerce[S, A]] = schema.self

    given [S[a] <: Schema[?, a]]: Invariant[Schema.Coerce[S, *]] =
      Invariant[[a] =>> Annotation[Base.Coerce[S, a]]].imapK([A] =>
        (annotation: Annotation[Base.Coerce[S, A]]) => Coerce(annotation)
      )([A] => (schema: Coerce[S, A]) => schema.self)

    given [S[a] <: Schema[?, a], A]: Annotated[Schema.Coerce[S, A]] =
      Annotated[Annotation[Base.Coerce[S, A]]].imap(Coerce.apply)(_.self)

    given Self.Coerce[Schema.Coerce, Schema[?, *]] = Self
      .Coerce[[s[a] <: Schema[?, a], a] =>> Annotation[Base.Coerce[s, a]], Schema[?, *]]
      .imapK([S[a] <: Schema[?, a], A] => (annotation: Annotation[Base.Coerce[S, A]]) => Coerce(annotation))(
        [S[a] <: Schema[?, a], A] => (schema: Coerce[S, A]) => schema.self
      )

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

      given [S[a] <: Schema.Read[?, a], A]: Annotated[Schema.Collection.Read[S, A]] =
        Annotated[Annotation[Base.Collection.Read[S, A]]].imap(Schema.Collection.Read.apply)(_.self)

      given [S[a] <: Schema.Read[?, a]]: Functor[Schema.Collection.Read[S, *]] =
        Functor[[a] =>> Annotation[Base.Collection.Read[S, a]]].imapK([A] =>
          (annotation: Annotation[Base.Collection.Read[S, A]]) => Schema.Collection.Read(annotation)
        )([A] => (schema: Schema.Collection.Read[S, A]) => schema.self)

      given Self.Collection.Read[Schema.Collection.Read, Schema.Read[?, *]] = Self.Collection
        .Read[[s[a] <: Schema.Read[?, a], a] =>> Annotation[Base.Collection.Read[s, a]], Schema.Read[?, *]]
        .imapK([S[a] <: Schema.Read[?, a], A] =>
          (annotation: Annotation[Base.Collection.Read[S, A]]) => Read(annotation)
        )([S[a] <: Schema.Read[?, a], A] => (schema: Schema.Collection.Read[S, A]) => schema.self)

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

      given [S[a] <: Schema.Write[?, a], A]: Annotated[Schema.Collection.Write[S, A]] =
        Annotated[Annotation[Base.Collection.Write[S, A]]].imap(Schema.Collection.Write.apply)(_.self)

      given [S[a] <: Schema.Write[?, a]]: Contravariant[Schema.Collection.Write[S, *]] =
        Contravariant[[a] =>> Annotation[Base.Collection.Write[S, a]]].imapK([A] =>
          (annotation: Annotation[Base.Collection.Write[S, A]]) => Schema.Collection.Write(annotation)
        )([A] => (schema: Schema.Collection.Write[S, A]) => schema.self)

      given Self.Collection.Write[Schema.Collection.Write, Schema.Write[?, *]] = Self.Collection
        .Write[[s[a] <: Schema.Write[?, a], a] =>> Annotation[Base.Collection.Write[s, a]], Schema.Write[?, *]]
        .imapK([S[a] <: Schema.Write[?, a], A] =>
          (annotation: Annotation[Base.Collection.Write[S, A]]) => Write(annotation)
        )([S[a] <: Schema.Write[?, a], A] => (schema: Schema.Collection.Write[S, A]) => schema.self)

    def apply[S[a] <: Schema[?, a], A](annotation: Annotation[Base.Collection[S, A]]): Schema.Collection[S, A] =
      new Collection[S, A]:
        override def self: Annotation[Base.Collection[S, A]] = annotation

    def unapply[S[a] <: Schema[?, a], A](schema: Schema.Collection[S, A]): Annotation[Base.Collection[S, A]] =
      schema.self

    given [S[a] <: Schema[?, a], A]: Annotated[Schema.Collection[S, A]] =
      Annotated[Annotation[Base.Collection[S, A]]].imap(Schema.Collection.apply)(_.self)

    given [S[a] <: Schema[?, a]]: Invariant[Schema.Collection[S, *]] =
      Invariant[[a] =>> Annotation[Base.Collection[S, a]]].imapK([A] =>
        (annotation: Annotation[Base.Collection[S, A]]) => Schema.Collection(annotation)
      )([A] => (schema: Schema.Collection[S, A]) => schema.self)

    given Self.Collection[Schema.Collection, Schema[?, *]] = Self
      .Collection[[s[a] <: Schema[?, a], a] =>> Annotation[Base.Collection[s, a]], Schema[?, *]]
      .imapK([S[a] <: Schema[?, a], A] => (annotation: Annotation[Base.Collection[S, A]]) => Collection(annotation))(
        [S[a] <: Schema[?, a], A] => (schema: Collection[S, A]) => schema.self
      )

  sealed abstract class Constant[+S[a] <: Schema[?, a], A]
      extends Schema[S, A],
        Schema.Constant.Read[S, A],
        Schema.Constant.Write[S, A]:
    override def self: Annotation[Base.Constant[S, A]]

  object Constant:
    sealed trait Read[+S[a] <: Schema.Read[?, a], +A] extends Schema.Read[S, A]:
      override def self: Annotation[Base.Constant.Read[S, A]]

    object Read:
      def apply[S[a] <: Schema.Read[?, a], A](
          annotation: Annotation[Base.Constant.Read[S, A]]
      ): Schema.Constant.Read[S, A] = new Read[S, A]:
        override def self: Annotation[Base.Constant.Read[S, A]] = annotation

      def unapply[S[a] <: Schema.Read[?, a], A](
          schema: Schema.Constant.Read[S, A]
      ): Annotation[Base.Constant.Read[S, A]] = schema.self

      given [S[a] <: Schema.Read[?, a], A]: Annotated[Schema.Constant.Read[S, A]] =
        Annotated[Annotation[Base.Constant.Read[S, A]]].imap(Schema.Constant.Read.apply)(_.self)

      given [S[a] <: Schema.Read[?, a]]: Functor[Schema.Constant.Read[S, *]] =
        Functor[[a] =>> Annotation[Base.Constant.Read[S, a]]].imapK([A] =>
          (annotation: Annotation[Base.Constant.Read[S, A]]) => Schema.Constant.Read(annotation)
        )([A] => (schema: Schema.Constant.Read[S, A]) => schema.self)

      given Self.Constant.Read[Schema.Constant.Read, Schema.Read[?, *]] = Self.Constant
        .Read[[s[a] <: Schema.Read[?, a], a] =>> Annotation[Base.Constant.Read[s, a]], Schema.Read[?, *]]
        .imapK([S[a] <: Schema.Read[?, a], A] =>
          (annotation: Annotation[Base.Constant.Read[S, A]]) => Read(annotation)
        )([S[a] <: Schema.Read[?, a], A] => (schema: Schema.Constant.Read[S, A]) => schema.self)

    sealed trait Write[+S[a] <: Schema.Write[?, a], -A] extends Schema.Write[S, A]:
      override def self: Annotation[Base.Constant.Write[S, A]]

    object Write:
      def apply[S[a] <: Schema.Write[?, a], A](
          annotation: Annotation[Base.Constant.Write[S, A]]
      ): Schema.Constant.Write[S, A] = new Write[S, A]:
        override def self: Annotation[Base.Constant.Write[S, A]] = annotation

      def unapply[S[a] <: Schema.Write[?, a], A](
          schema: Schema.Constant.Write[S, A]
      ): Annotation[Base.Constant.Write[S, A]] = schema.self

      given [S[a] <: Schema.Write[?, a], A]: Annotated[Schema.Constant.Write[S, A]] =
        Annotated[Annotation[Base.Constant.Write[S, A]]].imap(Schema.Constant.Write.apply)(_.self)

      given [S[a] <: Schema.Write[?, a]]: Contravariant[Schema.Constant.Write[S, *]] =
        Contravariant[[a] =>> Annotation[Base.Constant.Write[S, a]]].imapK([A] =>
          (annotation: Annotation[Base.Constant.Write[S, A]]) => Schema.Constant.Write(annotation)
        )([A] => (schema: Schema.Constant.Write[S, A]) => schema.self)

      given Self.Constant.Write[Schema.Constant.Write, Schema.Write[?, *]] = Self.Constant
        .Write[[s[a] <: Schema.Write[?, a], a] =>> Annotation[Base.Constant.Write[s, a]], Schema.Write[?, *]]
        .imapK([S[a] <: Schema.Write[?, a], A] =>
          (annotation: Annotation[Base.Constant.Write[S, A]]) => Write(annotation)
        )([S[a] <: Schema.Write[?, a], A] => (schema: Schema.Constant.Write[S, A]) => schema.self)

    def apply[S[a] <: Schema[?, a], A](annotation: Annotation[Base.Constant[S, A]]): Schema.Constant[S, A] =
      new Constant[S, A]:
        override def self: Annotation[Base.Constant[S, A]] = annotation

    def unapply[S[a] <: Schema[?, a], A](schema: Schema.Constant[S, A]): Annotation[Base.Constant[S, A]] =
      schema.self

    given [S[a] <: Schema[?, a], A]: Annotated[Schema.Constant[S, A]] =
      Annotated[Annotation[Base.Constant[S, A]]].imap(Schema.Constant.apply)(_.self)

    given [S[a] <: Schema[?, a]]: Invariant[Schema.Constant[S, *]] =
      Invariant[[a] =>> Annotation[Base.Constant[S, a]]].imapK([A] =>
        (annotation: Annotation[Base.Constant[S, A]]) => Schema.Constant(annotation)
      )([A] => (schema: Schema.Constant[S, A]) => schema.self)

    given Self.Constant[Schema.Constant, Schema[?, *]] = Self
      .Constant[[s[a] <: Schema[?, a], a] =>> Annotation[Base.Constant[s, a]], Schema[?, *]]
      .imapK([S[a] <: Schema[?, a], A] => (annotation: Annotation[Base.Constant[S, A]]) => Constant(annotation))(
        [S[a] <: Schema[?, a], A] => (schema: Constant[S, A]) => schema.self
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

      given [S[a] <: Schema.Read[?, a], A]: Annotated[Schema.Dictionary.Read[S, A]] =
        Annotated[Annotation[Base.Dictionary.Read[S, A]]].imap(Schema.Dictionary.Read.apply)(_.self)

      given [S[a] <: Schema.Read[?, a]]: Functor[Schema.Dictionary.Read[S, *]] =
        Functor[[a] =>> Annotation[Base.Dictionary.Read[S, a]]].imapK([A] =>
          (annotation: Annotation[Base.Dictionary.Read[S, A]]) => Schema.Dictionary.Read(annotation)
        )([A] => (schema: Schema.Dictionary.Read[S, A]) => schema.self)

      given Self.Dictionary.Read[Schema.Dictionary.Read, Schema.Read[?, *]] = Self.Dictionary
        .Read[[s[a] <: Schema.Read[?, a], a] =>> Annotation[Base.Dictionary.Read[s, a]], Schema.Read[?, *]]
        .imapK([S[a] <: Schema.Read[?, a], A] =>
          (annotation: Annotation[Base.Dictionary.Read[S, A]]) => Read(annotation)
        )([S[a] <: Schema.Read[?, a], A] => (schema: Schema.Dictionary.Read[S, A]) => schema.self)

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

      given [S[a] <: Schema.Write[?, a], A]: Annotated[Schema.Dictionary.Write[S, A]] =
        Annotated[Annotation[Base.Dictionary.Write[S, A]]].imap(Schema.Dictionary.Write.apply)(_.self)

      given [S[a] <: Schema.Write[?, a]]: Contravariant[Schema.Dictionary.Write[S, *]] =
        Contravariant[[a] =>> Annotation[Base.Dictionary.Write[S, a]]].imapK([A] =>
          (annotation: Annotation[Base.Dictionary.Write[S, A]]) => Schema.Dictionary.Write(annotation)
        )([A] => (schema: Schema.Dictionary.Write[S, A]) => schema.self)

      given Self.Dictionary.Write[Schema.Dictionary.Write, Schema.Write[?, *]] = Self.Dictionary
        .Write[[s[a] <: Schema.Write[?, a], a] =>> Annotation[Base.Dictionary.Write[s, a]], Schema.Write[?, *]]
        .imapK([S[a] <: Schema.Write[?, a], A] =>
          (annotation: Annotation[Base.Dictionary.Write[S, A]]) => Write(annotation)
        )([S[a] <: Schema.Write[?, a], A] => (schema: Schema.Dictionary.Write[S, A]) => schema.self)

    def apply[S[a] <: Schema[?, a], A](annotation: Annotation[Base.Dictionary[S, A]]): Schema.Dictionary[S, A] =
      new Dictionary[S, A]:
        override def self: Annotation[Base.Dictionary[S, A]] = annotation

    def unapply[S[a] <: Schema[?, a], A](schema: Schema.Dictionary[S, A]): Annotation[Base.Dictionary[S, A]] =
      schema.self

    given [S[a] <: Schema[?, a], A]: Annotated[Schema.Dictionary[S, A]] =
      Annotated[Annotation[Base.Dictionary[S, A]]].imap(Schema.Dictionary.apply)(_.self)

    given [S[a] <: Schema[?, a]]: Invariant[Schema.Dictionary[S, *]] =
      Invariant[[a] =>> Annotation[Base.Dictionary[S, a]]].imapK([A] =>
        (annotation: Annotation[Base.Dictionary[S, A]]) => Schema.Dictionary(annotation)
      )([A] => (schema: Schema.Dictionary[S, A]) => schema.self)

    given Self.Dictionary[Schema.Dictionary, Schema[?, *]] = Self
      .Dictionary[[s[a] <: Schema[?, a], a] =>> Annotation[Base.Dictionary[s, a]], Schema[?, *]]
      .imapK([S[a] <: Schema[?, a], A] => (annotation: Annotation[Base.Dictionary[S, A]]) => Dictionary(annotation))(
        [S[a] <: Schema[?, a], A] => (schema: Dictionary[S, A]) => schema.self
      )

  sealed abstract class Enumeration[+S[a] <: Schema[?, a], A]
      extends Schema[S, A],
        Schema.Enumeration.Read[S, A],
        Schema.Enumeration.Write[S, A]:
    override def self: Annotation[Base.Enumeration[S, A]]

  object Enumeration:
    sealed trait Read[+S[a] <: Schema.Read[?, a], +A] extends Schema.Read[S, A]:
      override def self: Annotation[Base.Enumeration.Read[S, A]]

    object Read:
      def apply[S[a] <: Schema.Read[?, a], A](
          annotation: Annotation[Base.Enumeration.Read[S, A]]
      ): Schema.Enumeration.Read[S, A] = new Read[S, A]:
        override def self: Annotation[Base.Enumeration.Read[S, A]] = annotation

      def unapply[S[a] <: Schema.Read[?, a], A](
          schema: Schema.Enumeration.Read[S, A]
      ): Annotation[Base.Enumeration.Read[S, A]] = schema.self

      given [S[a] <: Schema.Read[?, a], A]: Annotated[Schema.Enumeration.Read[S, A]] =
        Annotated[Annotation[Base.Enumeration.Read[S, A]]].imap(Schema.Enumeration.Read.apply)(_.self)

      given [S[a] <: Schema.Read[?, a]]: Functor[Schema.Enumeration.Read[S, *]] =
        Functor[[a] =>> Annotation[Base.Enumeration.Read[S, a]]].imapK([A] =>
          (annotation: Annotation[Base.Enumeration.Read[S, A]]) => Schema.Enumeration.Read(annotation)
        )([A] => (schema: Schema.Enumeration.Read[S, A]) => schema.self)

      given Self.Enumeration.Read[Schema.Enumeration.Read, Schema.Read[?, *]] = Self.Enumeration
        .Read[[s[a] <: Schema.Read[?, a], a] =>> Annotation[Base.Enumeration.Read[s, a]], Schema.Read[?, *]]
        .imapK([S[a] <: Schema.Read[?, a], A] =>
          (annotation: Annotation[Base.Enumeration.Read[S, A]]) => Read(annotation)
        )([S[a] <: Schema.Read[?, a], A] => (schema: Schema.Enumeration.Read[S, A]) => schema.self)

    sealed trait Write[+S[a] <: Schema.Write[?, a], -A] extends Schema.Write[S, A]:
      override def self: Annotation[Base.Enumeration.Write[S, A]]

    object Write:
      def apply[S[a] <: Schema.Write[?, a], A](
          annotation: Annotation[Base.Enumeration.Write[S, A]]
      ): Schema.Enumeration.Write[S, A] = new Write[S, A]:
        override def self: Annotation[Base.Enumeration.Write[S, A]] = annotation

      def unapply[S[a] <: Schema.Write[?, a], A](
          schema: Schema.Enumeration.Write[S, A]
      ): Annotation[Base.Enumeration.Write[S, A]] = schema.self

      given [S[a] <: Schema.Write[?, a], A]: Annotated[Schema.Enumeration.Write[S, A]] =
        Annotated[Annotation[Base.Enumeration.Write[S, A]]].imap(Schema.Enumeration.Write.apply)(_.self)

      given [S[a] <: Schema.Write[?, a]]: Contravariant[Schema.Enumeration.Write[S, *]] =
        Contravariant[[a] =>> Annotation[Base.Enumeration.Write[S, a]]].imapK([A] =>
          (annotation: Annotation[Base.Enumeration.Write[S, A]]) => Schema.Enumeration.Write(annotation)
        )([A] => (schema: Schema.Enumeration.Write[S, A]) => schema.self)

      given Self.Enumeration.Write[Schema.Enumeration.Write, Schema.Write[?, *]] = Self.Enumeration
        .Write[[s[a] <: Schema.Write[?, a], a] =>> Annotation[Base.Enumeration.Write[s, a]], Schema.Write[?, *]]
        .imapK([S[a] <: Schema.Write[?, a], A] =>
          (annotation: Annotation[Base.Enumeration.Write[S, A]]) => Write(annotation)
        )([S[a] <: Schema.Write[?, a], A] => (schema: Schema.Enumeration.Write[S, A]) => schema.self)

    def apply[S[a] <: Schema[?, a], A](annotation: Annotation[Base.Enumeration[S, A]]): Schema.Enumeration[S, A] =
      new Enumeration[S, A]:
        override def self: Annotation[Base.Enumeration[S, A]] = annotation

    def unapply[S[a] <: Schema[?, a], A](schema: Schema.Enumeration[S, A]): Annotation[Base.Enumeration[S, A]] =
      schema.self

    given [S[a] <: Schema[?, a], A]: Annotated[Schema.Enumeration[S, A]] =
      Annotated[Annotation[Base.Enumeration[S, A]]].imap(Schema.Enumeration.apply)(_.self)

    given [S[a] <: Schema[?, a]]: Invariant[Schema.Enumeration[S, *]] =
      Invariant[[a] =>> Annotation[Base.Enumeration[S, a]]].imapK([A] =>
        (annotation: Annotation[Base.Enumeration[S, A]]) => Schema.Enumeration(annotation)
      )([A] => (schema: Schema.Enumeration[S, A]) => schema.self)

    given Self.Enumeration[Schema.Enumeration, Schema[?, *]] = Self
      .Enumeration[[s[a] <: Schema[?, a], a] =>> Annotation[Base.Enumeration[s, a]], Schema[?, *]]
      .imapK([S[a] <: Schema[?, a], A] => (annotation: Annotation[Base.Enumeration[S, A]]) => Enumeration(annotation))(
        [S[a] <: Schema[?, a], A] => (schema: Enumeration[S, A]) => schema.self
      )

  sealed abstract class Nullish[+S[a] <: Schema[?, a], A]
      extends Schema[S, A],
        Schema.Nullish.Read[S, A],
        Schema.Nullish.Write[S, A]:
    override def self: Annotation[Base.Nullish[S, A]]

  object Nullish:
    sealed trait Read[+S[a] <: Schema.Read[?, a], +A] extends Schema.Read[S, A]:
      override def self: Annotation[Base.Nullish.Read[S, A]]

    object Read:
      def apply[S[a] <: Schema.Read[?, a], A](
          annotation: Annotation[Base.Nullish.Read[S, A]]
      ): Schema.Nullish.Read[S, A] = new Schema.Nullish.Read[S, A]:
        override def self: Annotation[Base.Nullish.Read[S, A]] = annotation

      def unapply[S[a] <: Schema.Read[?, a], A](
          schema: Schema.Nullish.Read[S, A]
      ): Annotation[Base.Nullish.Read[S, A]] = schema.self

      given [S[a] <: Schema.Read[?, a], A]: Annotated[Schema.Nullish.Read[S, A]] =
        Annotated[Annotation[Base.Nullish.Read[S, A]]].imap(Schema.Nullish.Read.apply)(_.self)

      given [S[a] <: Schema.Read[?, a]]: Functor[Schema.Nullish.Read[S, *]] =
        Functor[[a] =>> Annotation[Base.Nullish.Read[S, a]]].imapK([A] =>
          (annotation: Annotation[Base.Nullish.Read[S, A]]) => Schema.Nullish.Read(annotation)
        )([A] => (schema: Schema.Nullish.Read[S, A]) => schema.self)

      given Self.Nullish.Read[Schema.Nullish.Read, Schema.Read[?, *]] = Self.Nullish
        .Read[[s[a] <: Schema.Read[?, a], a] =>> Annotation[Base.Nullish.Read[s, a]], Schema.Read[?, *]]
        .imapK([S[a] <: Schema.Read[?, a], A] => (annotation: Annotation[Base.Nullish.Read[S, A]]) => Read(annotation))(
          [S[a] <: Schema.Read[?, a], A] => (schema: Schema.Nullish.Read[S, A]) => schema.self
        )

    sealed trait Write[+S[a] <: Schema.Write[?, a], -A] extends Schema.Write[S, A]:
      override def self: Annotation[Base.Nullish.Write[S, A]]

    object Write:
      def apply[S[a] <: Schema.Write[?, a], A](
          annotation: Annotation[Base.Nullish.Write[S, A]]
      ): Schema.Nullish.Write[S, A] = new Schema.Nullish.Write[S, A]:
        override def self: Annotation[Base.Nullish.Write[S, A]] = annotation

      def unapply[S[a] <: Schema.Write[?, a], A](
          schema: Schema.Nullish.Write[S, A]
      ): Annotation[Base.Nullish.Write[S, A]] = schema.self

      given [S[a] <: Schema.Write[?, a], A]: Annotated[Schema.Nullish.Write[S, A]] =
        Annotated[Annotation[Base.Nullish.Write[S, A]]].imap(Schema.Nullish.Write.apply)(_.self)

      given [S[a] <: Schema.Write[?, a]]: Contravariant[Schema.Nullish.Write[S, *]] =
        Contravariant[[a] =>> Annotation[Base.Nullish.Write[S, a]]].imapK([A] =>
          (annotation: Annotation[Base.Nullish.Write[S, A]]) => Schema.Nullish.Write(annotation)
        )([A] => (schema: Schema.Nullish.Write[S, A]) => schema.self)

      given Self.Nullish.Write[Schema.Nullish.Write, Schema.Write[?, *]] = Self.Nullish
        .Write[[s[a] <: Schema.Write[?, a], a] =>> Annotation[Base.Nullish.Write[s, a]], Schema.Write[?, *]]
        .imapK([S[a] <: Schema.Write[?, a], A] =>
          (annotation: Annotation[Base.Nullish.Write[S, A]]) => Write(annotation)
        )([S[a] <: Schema.Write[?, a], A] => (schema: Schema.Nullish.Write[S, A]) => schema.self)

    def apply[S[a] <: Schema[?, a], A](annotation: Annotation[Base.Nullish[S, A]]): Schema.Nullish[S, A] =
      new Nullish[S, A]:
        override def self: Annotation[Base.Nullish[S, A]] = annotation

    def unapply[S[a] <: Schema[?, a], A](schema: Schema.Nullish[S, A]): Annotation[Base.Nullish[S, A]] =
      schema.self

    given [S[a] <: Schema[?, a], A]: Annotated[Schema.Nullish[S, A]] =
      Annotated[Annotation[Base.Nullish[S, A]]].imap(Schema.Nullish.apply)(_.self)

    given [S[a] <: Schema[?, a]]: Invariant[Schema.Nullish[S, *]] =
      Invariant[[a] =>> Annotation[Base.Nullish[S, a]]].imapK([A] =>
        (annotation: Annotation[Base.Nullish[S, A]]) => Schema.Nullish(annotation)
      )([A] => (schema: Schema.Nullish[S, A]) => schema.self)

    given Self.Nullish[Schema.Nullish, Schema[?, *]] = Self
      .Nullish[[s[a] <: Schema[?, a], a] =>> Annotation[Base.Nullish[s, a]], Schema[?, *]]
      .imapK([S[a] <: Schema[?, a], A] => (annotation: Annotation[Base.Nullish[S, A]]) => Nullish(annotation))(
        [S[a] <: Schema[?, a], A] => (schema: Nullish[S, A]) => schema.self
      )

  sealed abstract class Record[+S[a] <: Schema[?, a], A]
      extends Schema[S, A],
        Schema.Record.Read[S, A],
        Schema.Record.Write[S, A]:
    override def self: Annotation[Base.Record[S, A]]

  object Record:
    sealed trait Read[+S[a] <: Schema.Read[?, a], +A] extends Schema.Read[S, A]:
      override def self: Annotation[Base.Record.Read[S, A]]

    object Read:
      def apply[S[a] <: Schema.Read[?, a], A](
          annotation: Annotation[Base.Record.Read[S, A]]
      ): Schema.Record.Read[S, A] = new Schema.Record.Read[S, A]:
        override def self: Annotation[Base.Record.Read[S, A]] = annotation

      def unapply[S[a] <: Schema.Read[?, a], A](
          schema: Schema.Record.Read[S, A]
      ): Annotation[Base.Record.Read[S, A]] = schema.self

      given [S[a] <: Schema.Read[?, a], A]: Annotated[Schema.Record.Read[S, A]] =
        Annotated[Annotation[Base.Record.Read[S, A]]].imap(Schema.Record.Read.apply)(_.self)

      given [S[a] <: Schema.Read[?, a]]: Functor[Schema.Record.Read[S, *]] =
        Functor[[a] =>> Annotation[Base.Record.Read[S, a]]].imapK([A] =>
          (annotation: Annotation[Base.Record.Read[S, A]]) => Schema.Record.Read(annotation)
        )([A] => (schema: Schema.Record.Read[S, A]) => schema.self)

      given Self.Record.Read[Schema.Record.Read, Schema.Read[?, *]] = Self.Record
        .Read[[s[a] <: Schema.Read[?, a], a] =>> Annotation[Base.Record.Read[s, a]], Schema.Read[?, *]]
        .imapK([S[a] <: Schema.Read[?, a], A] => (annotation: Annotation[Base.Record.Read[S, A]]) => Read(annotation))(
          [S[a] <: Schema.Read[?, a], A] => (schema: Schema.Record.Read[S, A]) => schema.self
        )

    sealed trait Write[+S[a] <: Schema.Write[?, a], -A] extends Schema.Write[S, A]:
      override def self: Annotation[Base.Record.Write[S, A]]

    object Write:
      def apply[S[a] <: Schema.Write[?, a], A](
          annotation: Annotation[Base.Record.Write[S, A]]
      ): Schema.Record.Write[S, A] = new Schema.Record.Write[S, A]:
        override def self: Annotation[Base.Record.Write[S, A]] = annotation

      def unapply[S[a] <: Schema.Write[?, a], A](
          schema: Schema.Record.Write[S, A]
      ): Annotation[Base.Record.Write[S, A]] = schema.self

      given [S[a] <: Schema.Write[?, a], A]: Annotated[Schema.Record.Write[S, A]] =
        Annotated[Annotation[Base.Record.Write[S, A]]].imap(Schema.Record.Write.apply)(_.self)

      given [S[a] <: Schema.Write[?, a]]: Contravariant[Schema.Record.Write[S, *]] =
        Contravariant[[a] =>> Annotation[Base.Record.Write[S, a]]].imapK([A] =>
          (annotation: Annotation[Base.Record.Write[S, A]]) => Schema.Record.Write(annotation)
        )([A] => (schema: Schema.Record.Write[S, A]) => schema.self)

      given Self.Record.Write[Schema.Record.Write, Schema.Write[?, *]] = Self.Record
        .Write[[s[a] <: Schema.Write[?, a], a] =>> Annotation[Base.Record.Write[s, a]], Schema.Write[?, *]]
        .imapK([S[a] <: Schema.Write[?, a], A] =>
          (annotation: Annotation[Base.Record.Write[S, A]]) => Write(annotation)
        )([S[a] <: Schema.Write[?, a], A] => (schema: Schema.Record.Write[S, A]) => schema.self)

    def apply[S[a] <: Schema[?, a], A](annotation: Annotation[Base.Record[S, A]]): Schema.Record[S, A] =
      new Record[S, A]:
        override def self: Annotation[Base.Record[S, A]] = annotation

    def unapply[S[a] <: Schema[?, a], A](schema: Schema.Record[S, A]): Annotation[Base.Record[S, A]] =
      schema.self

    given [S[a] <: Schema[?, a], A]: Annotated[Schema.Record[S, A]] =
      Annotated[Annotation[Base.Record[S, A]]].imap(Schema.Record.apply)(_.self)

    given [S[a] <: Schema[?, a]]: Invariant[Schema.Record[S, *]] =
      Invariant[[a] =>> Annotation[Base.Record[S, a]]].imapK([A] =>
        (annotation: Annotation[Base.Record[S, A]]) => Schema.Record(annotation)
      )([A] => (schema: Schema.Record[S, A]) => schema.self)

    given Self.Record[Schema.Record, Schema[?, *]] = Self
      .Record[[s[a] <: Schema[?, a], a] =>> Annotation[Base.Record[s, a]], Schema[?, *]]
      .imapK([S[a] <: Schema[?, a], A] => (annotation: Annotation[Base.Record[S, A]]) => Record(annotation))(
        [S[a] <: Schema[?, a], A] => (schema: Record[S, A]) => schema.self
      )

  sealed abstract class Tuple[+S[a] <: Schema[?, a], A]
      extends Schema[S, A],
        Schema.Tuple.Read[S, A],
        Schema.Tuple.Write[S, A]:
    override def self: Annotation[Base.Tuple[S, A]]

  object Tuple:
    sealed trait Read[+S[a] <: Schema.Read[?, a], +A] extends Schema.Read[S, A]:
      override def self: Annotation[Base.Tuple.Read[S, A]]

    object Read:
      def apply[S[a] <: Schema.Read[?, a], A](
          annotation: Annotation[Base.Tuple.Read[S, A]]
      ): Schema.Tuple.Read[S, A] = new Schema.Tuple.Read[S, A]:
        override def self: Annotation[Base.Tuple.Read[S, A]] = annotation

      def unapply[S[a] <: Schema.Read[?, a], A](
          schema: Schema.Tuple.Read[S, A]
      ): Annotation[Base.Tuple.Read[S, A]] = schema.self

      given [S[a] <: Schema.Read[?, a], A]: Annotated[Schema.Tuple.Read[S, A]] =
        Annotated[Annotation[Base.Tuple.Read[S, A]]].imap(Schema.Tuple.Read.apply)(_.self)

      given [S[a] <: Schema.Read[?, a]]: Functor[Schema.Tuple.Read[S, *]] =
        Functor[[a] =>> Annotation[Base.Tuple.Read[S, a]]].imapK([A] =>
          (annotation: Annotation[Base.Tuple.Read[S, A]]) => Schema.Tuple.Read(annotation)
        )([A] => (schema: Schema.Tuple.Read[S, A]) => schema.self)

      given Self.Tuple.Read[Schema.Tuple.Read, Schema.Read[?, *]] = Self.Tuple
        .Read[[s[a] <: Schema.Read[?, a], a] =>> Annotation[Base.Tuple.Read[s, a]], Schema.Read[?, *]]
        .imapK([S[a] <: Schema.Read[?, a], A] => (annotation: Annotation[Base.Tuple.Read[S, A]]) => Read(annotation))(
          [S[a] <: Schema.Read[?, a], A] => (schema: Schema.Tuple.Read[S, A]) => schema.self
        )

    sealed trait Write[+S[a] <: Schema.Write[?, a], -A] extends Schema.Write[S, A]:
      override def self: Annotation[Base.Tuple.Write[S, A]]

    object Write:
      def apply[S[a] <: Schema.Write[?, a], A](
          annotation: Annotation[Base.Tuple.Write[S, A]]
      ): Schema.Tuple.Write[S, A] = new Schema.Tuple.Write[S, A]:
        override def self: Annotation[Base.Tuple.Write[S, A]] = annotation

      def unapply[S[a] <: Schema.Write[?, a], A](
          schema: Schema.Tuple.Write[S, A]
      ): Annotation[Base.Tuple.Write[S, A]] = schema.self

      given [S[a] <: Schema.Write[?, a], A]: Annotated[Schema.Tuple.Write[S, A]] =
        Annotated[Annotation[Base.Tuple.Write[S, A]]].imap(Schema.Tuple.Write.apply)(_.self)

      given [S[a] <: Schema.Write[?, a]]: Contravariant[Schema.Tuple.Write[S, *]] =
        Contravariant[[a] =>> Annotation[Base.Tuple.Write[S, a]]].imapK([A] =>
          (annotation: Annotation[Base.Tuple.Write[S, A]]) => Schema.Tuple.Write(annotation)
        )([A] => (schema: Schema.Tuple.Write[S, A]) => schema.self)

      given Self.Tuple.Write[Schema.Tuple.Write, Schema.Write[?, *]] = Self.Tuple
        .Write[[s[a] <: Schema.Write[?, a], a] =>> Annotation[Base.Tuple.Write[s, a]], Schema.Write[?, *]]
        .imapK([S[a] <: Schema.Write[?, a], A] =>
          (annotation: Annotation[Base.Tuple.Write[S, A]]) => Write(annotation)
        )([S[a] <: Schema.Write[?, a], A] => (schema: Schema.Tuple.Write[S, A]) => schema.self)

    def apply[S[a] <: Schema[?, a], A](annotation: Annotation[Base.Tuple[S, A]]): Schema.Tuple[S, A] =
      new Tuple[S, A]:
        override def self: Annotation[Base.Tuple[S, A]] = annotation

    def unapply[S[a] <: Schema[?, a], A](schema: Schema.Tuple[S, A]): Annotation[Base.Tuple[S, A]] =
      schema.self

    given [S[a] <: Schema[?, a], A]: Annotated[Schema.Tuple[S, A]] =
      Annotated[Annotation[Base.Tuple[S, A]]].imap(Schema.Tuple.apply)(_.self)

    given [S[a] <: Schema[?, a]]: Invariant[Schema.Tuple[S, *]] =
      Invariant[[a] =>> Annotation[Base.Tuple[S, a]]].imapK([A] =>
        (annotation: Annotation[Base.Tuple[S, A]]) => Schema.Tuple(annotation)
      )([A] => (schema: Schema.Tuple[S, A]) => schema.self)

    given Self.Tuple[Schema.Tuple, Schema[?, *]] = Self
      .Tuple[[s[a] <: Schema[?, a], a] =>> Annotation[Base.Tuple[s, a]], Schema[?, *]]
      .imapK([S[a] <: Schema[?, a], A] => (annotation: Annotation[Base.Tuple[S, A]]) => Tuple(annotation))(
        [S[a] <: Schema[?, a], A] => (schema: Tuple[S, A]) => schema.self
      )

  sealed abstract class Primitive[A] extends Schema[Nothing, A], Schema.Primitive.Read[A], Schema.Primitive.Write[A]:
    override def self: Annotation[Base.Primitive[A]]

  object Primitive:
    sealed trait Read[+A] extends Schema.Read[Nothing, A]:
      override def self: Annotation[Base.Primitive.Read[A]]

    object Read:
      def apply[A](annotation: Annotation[Base.Primitive.Read[A]]): Schema.Primitive.Read[A] = annotation.self match
        case self: Base.Primitive.Boolean.Read[A] => Boolean.Read(annotation.copy(self = self))
        case self: Base.Primitive.Number.Read[A]  => Number.Read(annotation.copy(self = self))
        case self: Base.Primitive.Text.Read[A]    => Text.Read(annotation.copy(self = self))

      def unapply[A](schema: Schema.Primitive.Read[A]): Annotation[Base.Primitive.Read[A]] = schema.self

      given [A]: Annotated[Schema.Primitive.Read[A]] =
        Annotated[Annotation[Base.Primitive.Read[A]]].imap(Schema.Primitive.Read.apply)(_.self)

    sealed trait Write[-A] extends Schema.Write[Nothing, A]:
      override def self: Annotation[Base.Primitive.Write[A]]

    object Write:
      def apply[A](annotation: Annotation[Base.Primitive.Write[A]]): Schema.Primitive.Write[A] = annotation.self match
        case self: Base.Primitive.Boolean.Write[A] => Boolean.Write(annotation.copy(self = self))
        case self: Base.Primitive.Number.Write[A]  => Number.Write(annotation.copy(self = self))
        case self: Base.Primitive.Text.Write[A]    => Text.Write(annotation.copy(self = self))

      def unapply[A](schema: Schema.Primitive.Write[A]): Annotation[Base.Primitive.Write[A]] = schema.self

      given [A]: Annotated[Schema.Primitive.Write[A]] = Annotated[Annotation[Base.Primitive.Write[A]]]
        .imap(Schema.Primitive.Write.apply)(_.self)

    sealed abstract class Boolean[A]
        extends Schema.Primitive[A],
          Schema.Primitive.Boolean.Read[A],
          Schema.Primitive.Boolean.Write[A]:
      override def self: Annotation[Base.Primitive.Boolean[A]]

    object Boolean:
      sealed trait Read[+A] extends Schema.Primitive.Read[A]:
        override def self: Annotation[Base.Primitive.Boolean.Read[A]]

      object Read:
        def apply[A](annotation: Annotation[Base.Primitive.Boolean.Read[A]]): Schema.Primitive.Boolean.Read[A] =
          new Schema.Primitive.Boolean.Read[A]:
            override def self: Annotation[Base.Primitive.Boolean.Read[A]] = annotation

        def unapply[A](schema: Schema.Primitive.Boolean.Read[A]): Annotation[Base.Primitive.Boolean.Read[A]] =
          schema.self

        given [A]: Annotated[Schema.Primitive.Boolean.Read[A]] =
          Annotated[Annotation[Base.Primitive.Boolean.Read[A]]].imap(Schema.Primitive.Boolean.Read.apply)(_.self)

        given Functor[Schema.Primitive.Boolean.Read] =
          Functor[[a] =>> Annotation[Base.Primitive.Boolean.Read[a]]].imapK([A] =>
            (annotation: Annotation[Base.Primitive.Boolean.Read[A]]) => Schema.Primitive.Boolean.Read(annotation)
          )([A] => (schema: Schema.Primitive.Boolean.Read[A]) => schema.self)

      sealed trait Write[-A] extends Schema.Primitive.Write[A]:
        override def self: Annotation[Base.Primitive.Boolean.Write[A]]

      object Write:
        def apply[A](annotation: Annotation[Base.Primitive.Boolean.Write[A]]): Schema.Primitive.Boolean.Write[A] =
          new Schema.Primitive.Boolean.Write[A]:
            override def self: Annotation[Base.Primitive.Boolean.Write[A]] = annotation

        def unapply[A](schema: Schema.Primitive.Boolean.Write[A]): Annotation[Base.Primitive.Boolean.Write[A]] =
          schema.self

        given [A]: Annotated[Schema.Primitive.Boolean.Write[A]] =
          Annotated[Annotation[Base.Primitive.Boolean.Write[A]]].imap(Schema.Primitive.Boolean.Write.apply)(_.self)

        given Contravariant[Schema.Primitive.Boolean.Write] =
          Contravariant[[a] =>> Annotation[Base.Primitive.Boolean.Write[a]]].imapK([A] =>
            (annotation: Annotation[Base.Primitive.Boolean.Write[A]]) => Schema.Primitive.Boolean.Write(annotation)
          )([A] => (schema: Schema.Primitive.Boolean.Write[A]) => schema.self)

      def apply[A](annotation: Annotation[Base.Primitive.Boolean[A]]): Schema.Primitive.Boolean[A] = new Boolean[A]:
        override def self: Annotation[Base.Primitive.Boolean[A]] = annotation

      def unapply[A](schema: Schema.Primitive.Boolean[A]): Annotation[Base.Primitive.Boolean[A]] = schema.self

      given [A]: Annotated[Schema.Primitive.Boolean[A]] =
        Annotated[Annotation[Base.Primitive.Boolean[A]]].imap(Schema.Primitive.Boolean.apply)(_.self)

      given Invariant[Schema.Primitive.Boolean] =
        Invariant[[a] =>> Annotation[Base.Primitive.Boolean[a]]].imapK([A] =>
          (annotation: Annotation[Base.Primitive.Boolean[A]]) => Schema.Primitive.Boolean(annotation)
        )([A] => (schema: Schema.Primitive.Boolean[A]) => schema.self)

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

        given [A]: Annotated[Schema.Primitive.Number.Read[A]] =
          Annotated[Annotation[Base.Primitive.Number.Read[A]]].imap(Schema.Primitive.Number.Read.apply)(_.self)

        given Functor[Schema.Primitive.Number.Read] =
          Functor[[a] =>> Annotation[Base.Primitive.Number.Read[a]]].imapK([A] =>
            (annotation: Annotation[Base.Primitive.Number.Read[A]]) => Schema.Primitive.Number.Read(annotation)
          )([A] => (schema: Schema.Primitive.Number.Read[A]) => schema.self)

      sealed trait Write[-A] extends Schema.Primitive.Write[A]:
        override def self: Annotation[Base.Primitive.Number.Write[A]]

      object Write:
        def apply[A](annotation: Annotation[Base.Primitive.Number.Write[A]]): Schema.Primitive.Number.Write[A] =
          new Schema.Primitive.Number.Write[A]:
            override def self: Annotation[Base.Primitive.Number.Write[A]] = annotation

        def unapply[A](schema: Schema.Primitive.Number.Write[A]): Annotation[Base.Primitive.Number.Write[A]] =
          schema.self

        given [A]: Annotated[Schema.Primitive.Number.Write[A]] =
          Annotated[Annotation[Base.Primitive.Number.Write[A]]].imap(Schema.Primitive.Number.Write.apply)(_.self)

        given Contravariant[Schema.Primitive.Number.Write] =
          Contravariant[[a] =>> Annotation[Base.Primitive.Number.Write[a]]].imapK([A] =>
            (annotation: Annotation[Base.Primitive.Number.Write[A]]) => Schema.Primitive.Number.Write(annotation)
          )([A] => (schema: Schema.Primitive.Number.Write[A]) => schema.self)

      def apply[A](annotation: Annotation[Base.Primitive.Number[A]]): Schema.Primitive.Number[A] = new Number[A]:
        override def self: Annotation[Base.Primitive.Number[A]] = annotation

      def unapply[A](schema: Schema.Primitive.Number[A]): Annotation[Base.Primitive.Number[A]] = schema.self

      given [A]: Annotated[Schema.Primitive.Number[A]] =
        Annotated[Annotation[Base.Primitive.Number[A]]].imap(Schema.Primitive.Number.apply)(_.self)

      given Invariant[Schema.Primitive.Number] =
        Invariant[[a] =>> Annotation[Base.Primitive.Number[a]]].imapK([A] =>
          (annotation: Annotation[Base.Primitive.Number[A]]) => Schema.Primitive.Number(annotation)
        )([A] => (schema: Schema.Primitive.Number[A]) => schema.self)

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

        given [A]: Annotated[Schema.Primitive.Text.Read[A]] =
          Annotated[Annotation[Base.Primitive.Text.Read[A]]].imap(Schema.Primitive.Text.Read.apply)(_.self)

        given Functor[Schema.Primitive.Text.Read] =
          Functor[[a] =>> Annotation[Base.Primitive.Text.Read[a]]].imapK([A] =>
            (annotation: Annotation[Base.Primitive.Text.Read[A]]) => Schema.Primitive.Text.Read(annotation)
          )([A] => (schema: Schema.Primitive.Text.Read[A]) => schema.self)

      sealed trait Write[-A] extends Schema.Primitive.Write[A]:
        override def self: Annotation[Base.Primitive.Text.Write[A]]

      object Write:
        def apply[A](annotation: Annotation[Base.Primitive.Text.Write[A]]): Schema.Primitive.Text.Write[A] =
          new Schema.Primitive.Text.Write[A]:
            override def self: Annotation[Base.Primitive.Text.Write[A]] = annotation

        def unapply[A](schema: Schema.Primitive.Text.Write[A]): Annotation[Base.Primitive.Text.Write[A]] = schema.self

        given [A]: Annotated[Schema.Primitive.Text.Write[A]] =
          Annotated[Annotation[Base.Primitive.Text.Write[A]]].imap(Schema.Primitive.Text.Write.apply)(_.self)

        given Contravariant[Schema.Primitive.Text.Write] =
          Contravariant[[a] =>> Annotation[Base.Primitive.Text.Write[a]]].imapK([A] =>
            (annotation: Annotation[Base.Primitive.Text.Write[A]]) => Schema.Primitive.Text.Write(annotation)
          )([A] => (schema: Schema.Primitive.Text.Write[A]) => schema.self)

      def apply[A](annotation: Annotation[Base.Primitive.Text[A]]): Schema.Primitive.Text[A] =
        new Schema.Primitive.Text[A]:
          override def self: Annotation[Base.Primitive.Text[A]] = annotation

      def unapply[A](schema: Schema.Primitive.Text[A]): Annotation[Base.Primitive.Text[A]] = schema.self

      given [A]: Annotated[Schema.Primitive.Text[A]] =
        Annotated[Annotation[Base.Primitive.Text[A]]].imap(Schema.Primitive.Text.apply)(_.self)

      given Invariant[Schema.Primitive.Text] =
        Invariant[[a] =>> Annotation[Base.Primitive.Text[a]]].imapK([A] =>
          (annotation: Annotation[Base.Primitive.Text[A]]) => Schema.Primitive.Text(annotation)
        )([A] => (schema: Schema.Primitive.Text[A]) => schema.self)

    def apply[A](annotation: Annotation[Base.Primitive[A]]): Schema.Primitive[A] = annotation.self match
      case self: Base.Primitive.Boolean[A] => Schema.Primitive.Boolean(annotation.copy(self = self))
      case self: Base.Primitive.Number[A]  => Schema.Primitive.Number(annotation.copy(self = self))
      case self: Base.Primitive.Text[A]    => Schema.Primitive.Text(annotation.copy(self = self))

    def unapply[A](schema: Schema.Primitive[A]): Annotation[Base.Primitive[A]] = schema.self

    given [A]: Annotated[Schema.Primitive[A]] =
      Annotated[Annotation[Base.Primitive[A]]].imap(Schema.Primitive.apply)(_.self)

  sealed abstract class Union[+S[a] <: Schema[?, a], A]
      extends Schema[S, A],
        Schema.Union.Read[S, A],
        Schema.Union.Write[S, A]:
    override def self: Annotation[Base.Union[S, A]]

  object Union:
    sealed trait Read[+S[a] <: Schema.Read[?, a], +A] extends Schema.Read[S, A]:
      override def self: Annotation[Base.Union.Read[S, A]]

    object Read:
      def apply[S[a] <: Schema.Read[?, a], A](
          annotation: Annotation[Base.Union.Read[S, A]]
      ): Schema.Union.Read[S, A] = new Schema.Union.Read[S, A]:
        override def self: Annotation[Base.Union.Read[S, A]] = annotation

      def unapply[S[a] <: Schema.Read[?, a], A](
          schema: Schema.Union.Read[S, A]
      ): Annotation[Base.Union.Read[S, A]] = schema.self

      given [S[a] <: Schema.Read[?, a], A]: Annotated[Schema.Union.Read[S, A]] =
        Annotated[Annotation[Base.Union.Read[S, A]]].imap(Schema.Union.Read.apply)(_.self)

      given [S[a] <: Schema.Read[?, a]]: Functor[Schema.Union.Read[S, *]] =
        Functor[[a] =>> Annotation[Base.Union.Read[S, a]]].imapK([A] =>
          (annotation: Annotation[Base.Union.Read[S, A]]) => Schema.Union.Read(annotation)
        )([A] => (schema: Schema.Union.Read[S, A]) => schema.self)

      given Self.Union.Read[Schema.Union.Read, Schema.Read[?, *]] = Self.Union
        .Read[[s[a] <: Schema.Read[?, a], a] =>> Annotation[Base.Union.Read[s, a]], Schema.Read[?, *]]
        .imapK([S[a] <: Schema.Read[?, a], A] => (annotation: Annotation[Base.Union.Read[S, A]]) => Read(annotation))(
          [S[a] <: Schema.Read[?, a], A] => (schema: Schema.Union.Read[S, A]) => schema.self
        )

    sealed trait Write[+S[a] <: Schema.Write[?, a], -A] extends Schema.Write[S, A]:
      override def self: Annotation[Base.Union.Write[S, A]]

    object Write:
      def apply[S[a] <: Schema.Write[?, a], A](
          annotation: Annotation[Base.Union.Write[S, A]]
      ): Schema.Union.Write[S, A] = new Schema.Union.Write[S, A]:
        override def self: Annotation[Base.Union.Write[S, A]] = annotation

      def unapply[S[a] <: Schema.Write[?, a], A](
          schema: Schema.Union.Write[S, A]
      ): Annotation[Base.Union.Write[S, A]] = schema.self

      given [S[a] <: Schema.Write[?, a], A]: Annotated[Schema.Union.Write[S, A]] =
        Annotated[Annotation[Base.Union.Write[S, A]]].imap(Schema.Union.Write.apply)(_.self)

      given [S[a] <: Schema.Write[?, a]]: Contravariant[Schema.Union.Write[S, *]] =
        Contravariant[[a] =>> Annotation[Base.Union.Write[S, a]]].imapK([A] =>
          (annotation: Annotation[Base.Union.Write[S, A]]) => Schema.Union.Write(annotation)
        )([A] => (schema: Schema.Union.Write[S, A]) => schema.self)

      given Self.Union.Write[Schema.Union.Write, Schema.Write[?, *]] = Self.Union
        .Write[[s[a] <: Schema.Write[?, a], a] =>> Annotation[Base.Union.Write[s, a]], Schema.Write[?, *]]
        .imapK([S[a] <: Schema.Write[?, a], A] =>
          (annotation: Annotation[Base.Union.Write[S, A]]) => Write(annotation)
        )([S[a] <: Schema.Write[?, a], A] => (schema: Schema.Union.Write[S, A]) => schema.self)

    def apply[S[a] <: Schema[?, a], A](annotation: Annotation[Base.Union[S, A]]): Schema.Union[S, A] =
      new Union[S, A]:
        override def self: Annotation[Base.Union[S, A]] = annotation

    def unapply[S[a] <: Schema[?, a], A](schema: Schema.Union[S, A]): Annotation[Base.Union[S, A]] =
      schema.self

    given [S[a] <: Schema[?, a], A]: Annotated[Schema.Union[S, A]] =
      Annotated[Annotation[Base.Union[S, A]]].imap(Schema.Union.apply)(_.self)

    given [S[a] <: Schema[?, a]]: Invariant[Schema.Union[S, *]] =
      Invariant[[a] =>> Annotation[Base.Union[S, a]]].imapK([A] =>
        (annotation: Annotation[Base.Union[S, A]]) => Schema.Union(annotation)
      )([A] => (schema: Schema.Union[S, A]) => schema.self)

    given Self.Union[Schema.Union, Schema[?, *]] = Self
      .Union[[s[a] <: Schema[?, a], a] =>> Annotation[Base.Union[s, a]], Schema[?, *]]
      .imapK([S[a] <: Schema[?, a], A] => (annotation: Annotation[Base.Union[S, A]]) => Union(annotation))(
        [S[a] <: Schema[?, a], A] => (schema: Union[S, A]) => schema.self
      )

  def apply[S[a] <: Schema[?, a], A](annotation: Annotation[Schema.Of[S, A]]): Schema[S, A] = annotation.self match
    case self: Base.Coerce[S, A]      => Schema.Coerce(annotation.copy(self = self))
    case self: Base.Collection[S, A]  => Schema.Collection(annotation.copy(self = self))
    case self: Base.Constant[S, A]    => Schema.Constant(annotation.copy(self = self))
    case self: Base.Dictionary[S, A]  => Schema.Dictionary(annotation.copy(self = self))
    case self: Base.Enumeration[S, A] => Schema.Enumeration(annotation.copy(self = self))
    case self: Base.Nullish[S, A]     => Schema.Nullish(annotation.copy(self = self))
    case self: Base.Primitive[A]      => Schema.Primitive(annotation.copy(self = self))
    case self: Base.Record[S, A]      => Schema.Record(annotation.copy(self = self))
    case self: Base.Tuple[S, A]       => Schema.Tuple(annotation.copy(self = self))
    case self: Base.Union[S, A]       => Schema.Union(annotation.copy(self = self))

  def unapply[S[a] <: Schema[?, a], A](schema: Schema[S, A]): Annotation[Schema.Of[S, A]] = schema.self

  given [S[a] <: Schema[?, a], A]: Annotated[Schema[S, A]] =
    Annotated[Annotation[Schema.Of[S, A]]].imap(Schema.apply)(_.self)

  given Coerceable[Schema.Coerce, Schema[?, *]] with
    override val coerce: Self.Coerce[Schema.Coerce, Schema[?, *]] = Self.Coerce[Schema.Coerce, Schema[?, *]]

  given Nullable[Schema.Nullish, Schema[?, *]] with
    override val nullish: Self.Nullish[Schema.Nullish, Schema[?, *]] = Self.Nullish[Schema.Nullish, Schema[?, *]]

  given Tupleable[Schema.Tuple, Schema[?, *]] with
    override val tuple: Self.Tuple[Schema.Tuple, Schema[?, *]] = Self.Tuple[Schema.Tuple, Schema[?, *]]
