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

    given [S[a] <: Schema.Read[?, a]]: Coerceable[Schema.Coerce.Read, S] with
      override val coerce: Self.Coerce.Read[Schema.Coerce.Read, S] = Self.Coerce.Read[Schema.Coerce.Read, S]

    given [S[a] <: Schema.Read[?, a]]: Nullable[Schema.Nullish.Read, S] with
      override val nullish: Self.Nullish.Read[Schema.Nullish.Read, S] = Self.Nullish.Read[Schema.Nullish.Read, S]

    given [S[a] <: Schema.Read[?, a]]: Tupleable[Schema.Tuple.Read, S] with
      override val tuple: Self.Tuple[Schema.Tuple.Read, S] = Self.Tuple[Schema.Tuple.Read, S]

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

    given [S[a] <: Schema.Write[?, a]]: Coerceable[Schema.Coerce.Write, S] with
      override val coerce: Self.Coerce.Write[Schema.Coerce.Write, S] = Self.Coerce.Write[Schema.Coerce.Write, S]

    given [S[a] <: Schema.Write[?, a]]: Nullable[Schema.Nullish.Write, S] with
      override val nullish: Self.Nullish.Write[Schema.Nullish.Write, S] =
        Self.Nullish.Write[Schema.Nullish.Write, S]

    given [S[a] <: Schema.Write[?, a]]: Tupleable[Schema.Tuple.Write, S] with
      override val tuple: Self.Tuple[Schema.Tuple.Write, S] = Self.Tuple[Schema.Tuple.Write, S]

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

      given [S[a] <: Schema.Read[?, a]]: Self.Coerce.Read[Schema.Coerce.Read, S] = Self.Coerce
        .Read[[s[a] <: S[a], a] =>> Annotation[Base.Coerce.Read[s, a]], S]
        .imapK([s[a] <: S[a], a] => (annotation: Annotation[Base.Coerce.Read[s, a]]) => Read(annotation))(
          [s[a] <: S[a], a] => (schema: Schema.Coerce.Read[s, a]) => schema.self
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

      given [S[a] <: Schema.Write[?, a]]: Self.Coerce.Write[Schema.Coerce.Write, S] = Self.Coerce
        .Write[[s[a] <: S[a], a] =>> Annotation[Base.Coerce.Write[s, a]], S]
        .imapK([s[a] <: S[a], a] => (annotation: Annotation[Base.Coerce.Write[s, a]]) => Write(annotation))(
          [s[a] <: S[a], a] => (schema: Schema.Coerce.Write[s, a]) => schema.self
        )

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

    given [S[a] <: Schema[?, a]]: Self.Coerce[Schema.Coerce, S] = Self
      .Coerce[[s[a] <: S[a], a] =>> Annotation[Base.Coerce[s, a]], S]
      .imapK([s[a] <: Schema[?, a], a] => (annotation: Annotation[Base.Coerce[s, a]]) => Coerce(annotation))(
        [s[a] <: Schema[?, a], a] => (schema: Coerce[s, a]) => schema.self
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

      given [S[a] <: Schema.Read[?, a]]: Self.Collection.Read[Schema.Collection.Read, S] = Self.Collection
        .Read[[s[a] <: S[a], a] =>> Annotation[Base.Collection.Read[s, a]], S]
        .imapK([s[a] <: S[a], a] => (annotation: Annotation[Base.Collection.Read[s, a]]) => Read(annotation))(
          [s[a] <: S[a], a] => (schema: Schema.Collection.Read[s, a]) => schema.self
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

      given [S[a] <: Schema.Write[?, a], A]: Annotated[Schema.Collection.Write[S, A]] =
        Annotated[Annotation[Base.Collection.Write[S, A]]].imap(Schema.Collection.Write.apply)(_.self)

      given [S[a] <: Schema.Write[?, a]]: Contravariant[Schema.Collection.Write[S, *]] =
        Contravariant[[a] =>> Annotation[Base.Collection.Write[S, a]]].imapK([A] =>
          (annotation: Annotation[Base.Collection.Write[S, A]]) => Schema.Collection.Write(annotation)
        )([A] => (schema: Schema.Collection.Write[S, A]) => schema.self)

      given [S[a] <: Schema.Write[?, a]]: Self.Collection.Write[Schema.Collection.Write, S] = Self.Collection
        .Write[[s[a] <: S[a], a] =>> Annotation[Base.Collection.Write[s, a]], S]
        .imapK([s[a] <: S[a], a] => (annotation: Annotation[Base.Collection.Write[s, a]]) => Write(annotation))(
          [s[a] <: S[a], a] => (schema: Schema.Collection.Write[s, a]) => schema.self
        )

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

    given [S[a] <: Schema[?, a]]: Self.Collection[Schema.Collection, S] = Self
      .Collection[[s[a] <: S[a], a] =>> Annotation[Base.Collection[s, a]], S]
      .imapK([s[a] <: S[a], a] => (annotation: Annotation[Base.Collection[s, a]]) => Collection(annotation))(
        [s[a] <: S[a], a] => (schema: Collection[s, a]) => schema.self
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

      given [S[a] <: Schema.Read[?, a]]: Self.Constant.Read[Schema.Constant.Read, S] = Self.Constant
        .Read[[s[a] <: S[a], a] =>> Annotation[Base.Constant.Read[s, a]], S]
        .imapK([s[a] <: S[a], a] => (annotation: Annotation[Base.Constant.Read[s, a]]) => Read(annotation))(
          [s[a] <: S[a], a] => (schema: Schema.Constant.Read[s, a]) => schema.self
        )

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

      given [S[a] <: Schema.Write[?, a]]: Self.Constant.Write[Schema.Constant.Write, S] = Self.Constant
        .Write[[s[a] <: S[a], a] =>> Annotation[Base.Constant.Write[s, a]], S]
        .imapK([s[a] <: S[a], a] => (annotation: Annotation[Base.Constant.Write[s, a]]) => Write(annotation))(
          [s[a] <: S[a], a] => (schema: Schema.Constant.Write[s, a]) => schema.self
        )

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

    given [S[a] <: Schema[?, a]]: Self.Constant[Schema.Constant, S] = Self
      .Constant[[s[a] <: S[a], a] =>> Annotation[Base.Constant[s, a]], S]
      .imapK([s[a] <: S[a], a] => (annotation: Annotation[Base.Constant[s, a]]) => Constant(annotation))(
        [s[a] <: S[a], a] => (schema: Constant[s, a]) => schema.self
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

      given [S[a] <: Schema.Read[?, a]]: Self.Dictionary.Read[Schema.Dictionary.Read, S] = Self.Dictionary
        .Read[[s[a] <: S[a], a] =>> Annotation[Base.Dictionary.Read[s, a]], S]
        .imapK([s[a] <: S[a], a] => (annotation: Annotation[Base.Dictionary.Read[s, a]]) => Read(annotation))(
          [s[a] <: S[a], a] => (schema: Schema.Dictionary.Read[s, a]) => schema.self
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

      given [S[a] <: Schema.Write[?, a], A]: Annotated[Schema.Dictionary.Write[S, A]] =
        Annotated[Annotation[Base.Dictionary.Write[S, A]]].imap(Schema.Dictionary.Write.apply)(_.self)

      given [S[a] <: Schema.Write[?, a]]: Contravariant[Schema.Dictionary.Write[S, *]] =
        Contravariant[[a] =>> Annotation[Base.Dictionary.Write[S, a]]].imapK([A] =>
          (annotation: Annotation[Base.Dictionary.Write[S, A]]) => Schema.Dictionary.Write(annotation)
        )([A] => (schema: Schema.Dictionary.Write[S, A]) => schema.self)

      given [S[a] <: Schema.Write[?, a]]: Self.Dictionary.Write[Schema.Dictionary.Write, S] = Self.Dictionary
        .Write[[s[a] <: S[a], a] =>> Annotation[Base.Dictionary.Write[s, a]], S]
        .imapK([s[a] <: S[a], a] => (annotation: Annotation[Base.Dictionary.Write[s, a]]) => Write(annotation))(
          [s[a] <: S[a], a] => (schema: Schema.Dictionary.Write[s, a]) => schema.self
        )

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

    given [S[a] <: Schema[?, a]]: Self.Dictionary[Schema.Dictionary, S] = Self
      .Dictionary[[s[a] <: S[a], a] =>> Annotation[Base.Dictionary[s, a]], S]
      .imapK([s[a] <: S[a], a] => (annotation: Annotation[Base.Dictionary[s, a]]) => Dictionary(annotation))(
        [s[a] <: S[a], a] => (schema: Dictionary[s, a]) => schema.self
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

      given [S[a] <: Schema.Read[?, a]]: Self.Enumeration.Read[Schema.Enumeration.Read, S] = Self.Enumeration
        .Read[[s[a] <: S[a], a] =>> Annotation[Base.Enumeration.Read[s, a]], S]
        .imapK([s[a] <: S[a], a] => (annotation: Annotation[Base.Enumeration.Read[s, a]]) => Read(annotation))(
          [s[a] <: S[a], a] => (schema: Schema.Enumeration.Read[s, a]) => schema.self
        )

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

      given [S[a] <: Schema.Write[?, a]]: Self.Enumeration.Write[Schema.Enumeration.Write, S] = Self.Enumeration
        .Write[[s[a] <: S[a], a] =>> Annotation[Base.Enumeration.Write[s, a]], S]
        .imapK([s[a] <: S[a], a] => (annotation: Annotation[Base.Enumeration.Write[s, a]]) => Write(annotation))(
          [s[a] <: S[a], a] => (schema: Schema.Enumeration.Write[s, a]) => schema.self
        )

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

    given [S[a] <: Schema[?, a]]: Self.Enumeration[Schema.Enumeration, S] = Self
      .Enumeration[[s[a] <: S[a], a] =>> Annotation[Base.Enumeration[s, a]], S]
      .imapK([s[a] <: S[a], a] => (annotation: Annotation[Base.Enumeration[s, a]]) => Enumeration(annotation))(
        [s[a] <: S[a], a] => (schema: Enumeration[s, a]) => schema.self
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

      given [S[a] <: Schema.Read[?, a]]: Self.Nullish.Read[Schema.Nullish.Read, S] = Self.Nullish
        .Read[[s[a] <: S[a], a] =>> Annotation[Base.Nullish.Read[s, a]], S]
        .imapK([s[a] <: S[a], a] => (annotation: Annotation[Base.Nullish.Read[s, a]]) => Read(annotation))(
          [s[a] <: S[a], a] => (schema: Schema.Nullish.Read[s, a]) => schema.self
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

      given [S[a] <: Schema.Write[?, a]]: Self.Nullish.Write[Schema.Nullish.Write, S] = Self.Nullish
        .Write[[s[a] <: S[a], a] =>> Annotation[Base.Nullish.Write[s, a]], S]
        .imapK([s[a] <: S[a], a] => (annotation: Annotation[Base.Nullish.Write[s, a]]) => Write(annotation))(
          [s[a] <: S[a], a] => (schema: Schema.Nullish.Write[s, a]) => schema.self
        )

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

    given [S[a] <: Schema[?, a]]: Self.Nullish[Schema.Nullish, S] = Self
      .Nullish[[s[a] <: S[a], a] =>> Annotation[Base.Nullish[s, a]], S]
      .imapK([s[a] <: S[a], a] => (annotation: Annotation[Base.Nullish[s, a]]) => Nullish(annotation))(
        [s[a] <: S[a], a] => (schema: Nullish[s, a]) => schema.self
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

      given [S[a] <: Schema.Read[?, a]]: Self.Record.Read[Schema.Record.Read, S] = Self.Record
        .Read[[s[a] <: S[a], a] =>> Annotation[Base.Record.Read[s, a]], S]
        .imapK([s[a] <: S[a], a] => (annotation: Annotation[Base.Record.Read[s, a]]) => Read(annotation))(
          [s[a] <: S[a], a] => (schema: Schema.Record.Read[s, a]) => schema.self
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

      given [S[a] <: Schema.Write[?, a]]: Self.Record.Write[Schema.Record.Write, S] = Self.Record
        .Write[[s[a] <: S[a], a] =>> Annotation[Base.Record.Write[s, a]], S]
        .imapK([s[a] <: S[a], a] => (annotation: Annotation[Base.Record.Write[s, a]]) => Write(annotation))(
          [s[a] <: S[a], a] => (schema: Schema.Record.Write[s, a]) => schema.self
        )

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

    given [S[a] <: Schema[?, a]]: Self.Record[Schema.Record, S] = Self
      .Record[[s[a] <: S[a], a] =>> Annotation[Base.Record[s, a]], S]
      .imapK([s[a] <: S[a], a] => (annotation: Annotation[Base.Record[s, a]]) => Record(annotation))(
        [s[a] <: S[a], a] => (schema: Record[s, a]) => schema.self
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

      given [S[a] <: Schema.Read[?, a]]: Self.Tuple.Read[Schema.Tuple.Read, S] = Self.Tuple
        .Read[[s[a] <: S[a], a] =>> Annotation[Base.Tuple.Read[s, a]], S]
        .imapK([s[a] <: S[a], a] => (annotation: Annotation[Base.Tuple.Read[s, a]]) => Read(annotation))(
          [s[a] <: S[a], a] => (schema: Schema.Tuple.Read[s, a]) => schema.self
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

      given [S[a] <: Schema.Write[?, a]]: Self.Tuple.Write[Schema.Tuple.Write, S] = Self.Tuple
        .Write[[s[a] <: S[a], a] =>> Annotation[Base.Tuple.Write[s, a]], S]
        .imapK([s[a] <: S[a], a] => (annotation: Annotation[Base.Tuple.Write[s, a]]) => Write(annotation))(
          [s[a] <: S[a], a] => (schema: Schema.Tuple.Write[s, a]) => schema.self
        )

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

    given [S[a] <: Schema[?, a]]: Self.Tuple[Schema.Tuple, S] = Self
      .Tuple[[s[a] <: S[a], a] =>> Annotation[Base.Tuple[s, a]], S]
      .imapK([s[a] <: S[a], a] => (annotation: Annotation[Base.Tuple[s, a]]) => Tuple(annotation))([s[a] <: S[a], a] =>
        (schema: Tuple[s, a]) => schema.self
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

      given [S[a] <: Schema.Read[?, a]]: Self.Union.Read[Schema.Union.Read, S] = Self.Union
        .Read[[s[a] <: S[a], a] =>> Annotation[Base.Union.Read[s, a]], S]
        .imapK([s[a] <: S[a], a] => (annotation: Annotation[Base.Union.Read[s, a]]) => Read(annotation))(
          [s[a] <: S[a], a] => (schema: Schema.Union.Read[s, a]) => schema.self
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

      given [S[a] <: Schema.Write[?, a]]: Self.Union.Write[Schema.Union.Write, S] = Self.Union
        .Write[[s[a] <: S[a], a] =>> Annotation[Base.Union.Write[s, a]], S]
        .imapK([s[a] <: S[a], a] => (annotation: Annotation[Base.Union.Write[s, a]]) => Write(annotation))(
          [s[a] <: S[a], a] => (schema: Schema.Union.Write[s, a]) => schema.self
        )

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

    given [S[a] <: Schema[?, a]]: Self.Union[Schema.Union, S] = Self
      .Union[[s[a] <: S[a], a] =>> Annotation[Base.Union[s, a]], S]
      .imapK([s[a] <: S[a], a] => (annotation: Annotation[Base.Union[s, a]]) => Union(annotation))([s[a] <: S[a], a] =>
        (schema: Union[s, a]) => schema.self
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

  given [S[a] <: Schema[?, a]]: Coerceable[Schema.Coerce, S] with
    override val coerce: Self.Coerce[Schema.Coerce, S] =
      Self.Coerce[Schema.Coerce, S]

  given [S[a] <: Schema[?, a]]: Nullable[Schema.Nullish, S] with
    override val nullish: Self.Nullish[Schema.Nullish, S] = Self.Nullish[Schema.Nullish, S]

  given [S[a] <: Schema[?, a]]: Tupleable[Schema.Tuple, S] with
    override val tuple: Self.Tuple[Schema.Tuple, S] = Self.Tuple[Schema.Tuple, S]
