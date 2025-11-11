package io.taig.otter

import cats.Contravariant
import cats.Functor
import cats.Invariant
import io.taig.otter as Self
import io.taig.otter.base as Base
import io.taig.otter.syntax.CatsSyntax.*

sealed abstract class Schema[+S[a] <: Schema[?, a], A] extends Schema.Read[S, A], Schema.Write[S, A]:
  override def self: Annotation[Schema.Of[S, A]]

object Schema:
  type Of[+S[_], A] = Base.CoerceBase[S, A] | Base.CollectionBase[S, A] | Base.ConstantBase[S, A] |
    Base.DictionaryBase[S, A] | Base.EnumerationBase[S, A] | Base.NullishBase[S, A] | Base.PrimitiveBase[A] |
    Base.RecordBase[S, A] | Base.TupleBase[S, A] | Base.UnionBase[S, A]

  sealed trait Read[+S[a] <: Schema.Read[?, a], +A]:
    def self: Annotation[Schema.Read.Of[S, A]]

  object Read:
    type Of[+S[_], +A] = Base.CoerceBase.Read[S, A] | Base.CollectionBase.Read[S, A] | Base.ConstantBase.Read[S, A] |
      Base.DictionaryBase.Read[S, A] | Base.EnumerationBase.Read[S, A] | Base.NullishBase.Read[S, A] |
      Base.PrimitiveBase.Read[A] | Base.RecordBase.Read[S, A] | Base.TupleBase.Read[S, A] | Base.UnionBase.Read[S, A]

    def apply[S[a] <: Schema.Read[?, a], A](annotation: Annotation[Schema.Read.Of[S, A]]): Schema.Read[S, A] =
      annotation.self match
        case self: Base.CoerceBase.Read[S, A]      => Schema.Coerce.Read(annotation.copy(self = self))
        case self: Base.CollectionBase.Read[S, A]  => Schema.Collection.Read(annotation.copy(self = self))
        case self: Base.ConstantBase.Read[S, A]    => Schema.Constant.Read(annotation.copy(self = self))
        case self: Base.DictionaryBase.Read[S, A]  => Schema.Dictionary.Read(annotation.copy(self = self))
        case self: Base.EnumerationBase.Read[S, A] => Schema.Enumeration.Read(annotation.copy(self = self))
        case self: Base.NullishBase.Read[S, A]     => Schema.Nullish.Read(annotation.copy(self = self))
        case self: Base.PrimitiveBase.Read[A]      => Schema.Primitive.Read(annotation.copy(self = self))
        case self: Base.RecordBase.Read[S, A]      => Schema.Record.Read(annotation.copy(self = self))
        case self: Base.TupleBase.Read[S, A]       => Schema.Tuple.Read(annotation.copy(self = self))
        case self: Base.UnionBase.Read[S, A]       => Schema.Union.Read(annotation.copy(self = self))

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
    type Of[+S[_], -A] = Base.CoerceBase.Write[S, A] | Base.CollectionBase.Write[S, A] | Base.ConstantBase.Write[S, A] |
      Base.DictionaryBase.Write[S, A] | Base.EnumerationBase.Write[S, A] | Base.NullishBase.Write[S, A] |
      Base.PrimitiveBase.Write[A] | Base.RecordBase.Write[S, A] | Base.TupleBase.Write[S, A] |
      Base.UnionBase.Write[S, A]

    def apply[S[a] <: Schema.Write[?, a], A](annotation: Annotation[Schema.Write.Of[S, A]]): Schema.Write[S, A] =
      annotation.self match
        case self: Base.CoerceBase.Write[S, A]      => Schema.Coerce.Write(annotation.copy(self = self))
        case self: Base.CollectionBase.Write[S, A]  => Schema.Collection.Write(annotation.copy(self = self))
        case self: Base.ConstantBase.Write[S, A]    => Schema.Constant.Write(annotation.copy(self = self))
        case self: Base.DictionaryBase.Write[S, A]  => Schema.Dictionary.Write(annotation.copy(self = self))
        case self: Base.EnumerationBase.Write[S, A] => Schema.Enumeration.Write(annotation.copy(self = self))
        case self: Base.NullishBase.Write[S, A]     => Schema.Nullish.Write(annotation.copy(self = self))
        case self: Base.PrimitiveBase.Write[A]      => Schema.Primitive.Write(annotation.copy(self = self))
        case self: Base.RecordBase.Write[S, A]      => Schema.Record.Write(annotation.copy(self = self))
        case self: Base.TupleBase.Write[S, A]       => Schema.Tuple.Write(annotation.copy(self = self))
        case self: Base.UnionBase.Write[S, A]       => Schema.Union.Write(annotation.copy(self = self))

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
    override def self: Annotation[Base.CoerceBase[S, A]]

  object Coerce:
    sealed trait Read[+S[a] <: Schema.Read[?, a], +A] extends Schema.Read[S, A]:
      override def self: Annotation[Base.CoerceBase.Read[S, A]]

    object Read:
      def apply[S[a] <: Schema.Read[?, a], A](
          annotation: Annotation[Base.CoerceBase.Read[S, A]]
      ): Schema.Coerce.Read[S, A] = new Read[S, A]:
        override def self: Annotation[Base.CoerceBase.Read[S, A]] = annotation

      def unapply[S[a] <: Schema.Read[?, a], A](
          schema: Schema.Coerce.Read[S, A]
      ): Annotation[Base.CoerceBase.Read[S, A]] =
        schema.self

      given [S[a] <: Schema.Read[?, a]]: Functor[Schema.Coerce.Read[S, *]] =
        Functor[[a] =>> Annotation[Base.CoerceBase.Read[S, a]]].imapK([A] =>
          (annotation: Annotation[Base.CoerceBase.Read[S, A]]) => Schema.Coerce.Read(annotation)
        )([A] => (schema: Schema.Coerce.Read[S, A]) => schema.self)

      given [S[a] <: Schema.Read[?, a], A]: Annotated[Schema.Coerce.Read[S, A]] =
        Annotated[Annotation[Base.CoerceBase.Read[S, A]]].imap(Schema.Coerce.Read.apply)(_.self)

      given [S[a] <: Schema.Read[?, a]]: Self.Coerce.Read[Schema.Coerce.Read, S] = Self.Coerce
        .Read[[s[a] <: S[a], a] =>> Annotation[Base.CoerceBase.Read[s, a]], S]
        .imapK([s[a] <: S[a], a] => (annotation: Annotation[Base.CoerceBase.Read[s, a]]) => Read(annotation))(
          [s[a] <: S[a], a] => (schema: Schema.Coerce.Read[s, a]) => schema.self
        )

    sealed trait Write[+S[a] <: Schema.Write[?, a], -A] extends Schema.Write[S, A]:
      override def self: Annotation[Base.CoerceBase.Write[S, A]]

    object Write:
      def apply[S[a] <: Schema.Write[?, a], A](
          annotation: Annotation[Base.CoerceBase.Write[S, A]]
      ): Schema.Coerce.Write[S, A] = new Write[S, A]:
        override def self: Annotation[Base.CoerceBase.Write[S, A]] = annotation

      def unapply[S[a] <: Schema.Write[?, a], A](
          schema: Schema.Coerce.Write[S, A]
      ): Annotation[Base.CoerceBase.Write[S, A]] = schema.self

      given [S[a] <: Schema.Write[?, a]]: Contravariant[Schema.Coerce.Write[S, *]] =
        Contravariant[[a] =>> Annotation[Base.CoerceBase.Write[S, a]]].imapK([A] =>
          (annotation: Annotation[Base.CoerceBase.Write[S, A]]) => Schema.Coerce.Write(annotation)
        )([A] => (schema: Schema.Coerce.Write[S, A]) => schema.self)

      given [S[a] <: Schema.Write[?, a], A]: Annotated[Schema.Coerce.Write[S, A]] =
        Annotated[Annotation[Base.CoerceBase.Write[S, A]]].imap(Schema.Coerce.Write.apply)(_.self)

      given [S[a] <: Schema.Write[?, a]]: Self.Coerce.Write[Schema.Coerce.Write, S] = Self.Coerce
        .Write[[s[a] <: S[a], a] =>> Annotation[Base.CoerceBase.Write[s, a]], S]
        .imapK([s[a] <: S[a], a] => (annotation: Annotation[Base.CoerceBase.Write[s, a]]) => Write(annotation))(
          [s[a] <: S[a], a] => (schema: Schema.Coerce.Write[s, a]) => schema.self
        )

    def apply[S[a] <: Schema[?, a], A](annotation: Annotation[Base.CoerceBase[S, A]]): Schema.Coerce[S, A] =
      new Coerce[S, A]:
        override def self: Annotation[Base.CoerceBase[S, A]] = annotation

    def unapply[S[a] <: Schema[?, a], A](schema: Schema.Coerce[S, A]): Annotation[Base.CoerceBase[S, A]] = schema.self

    given [S[a] <: Schema[?, a]]: Invariant[Schema.Coerce[S, *]] =
      Invariant[[a] =>> Annotation[Base.CoerceBase[S, a]]].imapK([A] =>
        (annotation: Annotation[Base.CoerceBase[S, A]]) => Coerce(annotation)
      )([A] => (schema: Coerce[S, A]) => schema.self)

    given [S[a] <: Schema[?, a], A]: Annotated[Schema.Coerce[S, A]] =
      Annotated[Annotation[Base.CoerceBase[S, A]]].imap(Coerce.apply)(_.self)

    given [S[a] <: Schema[?, a]]: Self.Coerce[Schema.Coerce, S] = Self
      .Coerce[[s[a] <: S[a], a] =>> Annotation[Base.CoerceBase[s, a]], S]
      .imapK([s[a] <: Schema[?, a], a] => (annotation: Annotation[Base.CoerceBase[s, a]]) => Coerce(annotation))(
        [s[a] <: Schema[?, a], a] => (schema: Coerce[s, a]) => schema.self
      )

  sealed abstract class Collection[+S[a] <: Schema[?, a], A]
      extends Schema[S, A],
        Schema.Collection.Read[S, A],
        Schema.Collection.Write[S, A]:
    override def self: Annotation[Base.CollectionBase[S, A]]

  object Collection:
    sealed trait Read[+S[a] <: Schema.Read[?, a], +A] extends Schema.Read[S, A]:
      override def self: Annotation[Base.CollectionBase.Read[S, A]]

    object Read:
      def apply[S[a] <: Schema.Read[?, a], A](
          annotation: Annotation[Base.CollectionBase.Read[S, A]]
      ): Schema.Collection.Read[S, A] = new Read[S, A]:
        override def self: Annotation[Base.CollectionBase.Read[S, A]] = annotation

      def unapply[S[a] <: Schema.Read[?, a], A](
          schema: Schema.Collection.Read[S, A]
      ): Annotation[Base.CollectionBase.Read[S, A]] = schema.self

      given [S[a] <: Schema.Read[?, a], A]: Annotated[Schema.Collection.Read[S, A]] =
        Annotated[Annotation[Base.CollectionBase.Read[S, A]]].imap(Schema.Collection.Read.apply)(_.self)

      given [S[a] <: Schema.Read[?, a]]: Functor[Schema.Collection.Read[S, *]] =
        Functor[[a] =>> Annotation[Base.CollectionBase.Read[S, a]]].imapK([A] =>
          (annotation: Annotation[Base.CollectionBase.Read[S, A]]) => Schema.Collection.Read(annotation)
        )([A] => (schema: Schema.Collection.Read[S, A]) => schema.self)

      given [S[a] <: Schema.Read[?, a]]: Self.Collection.Read[Schema.Collection.Read, S] = Self.Collection
        .Read[[s[a] <: S[a], a] =>> Annotation[Base.CollectionBase.Read[s, a]], S]
        .imapK([s[a] <: S[a], a] => (annotation: Annotation[Base.CollectionBase.Read[s, a]]) => Read(annotation))(
          [s[a] <: S[a], a] => (schema: Schema.Collection.Read[s, a]) => schema.self
        )

    sealed trait Write[+S[a] <: Schema.Write[?, a], -A] extends Schema.Write[S, A]:
      override def self: Annotation[Base.CollectionBase.Write[S, A]]

    object Write:
      def apply[S[a] <: Schema.Write[?, a], A](
          annotation: Annotation[Base.CollectionBase.Write[S, A]]
      ): Schema.Collection.Write[S, A] = new Write[S, A]:
        override def self: Annotation[Base.CollectionBase.Write[S, A]] = annotation

      def unapply[S[a] <: Schema.Write[?, a], A](
          schema: Schema.Collection.Write[S, A]
      ): Annotation[Base.CollectionBase.Write[S, A]] = schema.self

      given [S[a] <: Schema.Write[?, a], A]: Annotated[Schema.Collection.Write[S, A]] =
        Annotated[Annotation[Base.CollectionBase.Write[S, A]]].imap(Schema.Collection.Write.apply)(_.self)

      given [S[a] <: Schema.Write[?, a]]: Contravariant[Schema.Collection.Write[S, *]] =
        Contravariant[[a] =>> Annotation[Base.CollectionBase.Write[S, a]]].imapK([A] =>
          (annotation: Annotation[Base.CollectionBase.Write[S, A]]) => Schema.Collection.Write(annotation)
        )([A] => (schema: Schema.Collection.Write[S, A]) => schema.self)

      given [S[a] <: Schema.Write[?, a]]: Self.Collection.Write[Schema.Collection.Write, S] = Self.Collection
        .Write[[s[a] <: S[a], a] =>> Annotation[Base.CollectionBase.Write[s, a]], S]
        .imapK([s[a] <: S[a], a] => (annotation: Annotation[Base.CollectionBase.Write[s, a]]) => Write(annotation))(
          [s[a] <: S[a], a] => (schema: Schema.Collection.Write[s, a]) => schema.self
        )

    def apply[S[a] <: Schema[?, a], A](annotation: Annotation[Base.CollectionBase[S, A]]): Schema.Collection[S, A] =
      new Collection[S, A]:
        override def self: Annotation[Base.CollectionBase[S, A]] = annotation

    def unapply[S[a] <: Schema[?, a], A](schema: Schema.Collection[S, A]): Annotation[Base.CollectionBase[S, A]] =
      schema.self

    given [S[a] <: Schema[?, a], A]: Annotated[Schema.Collection[S, A]] =
      Annotated[Annotation[Base.CollectionBase[S, A]]].imap(Schema.Collection.apply)(_.self)

    given [S[a] <: Schema[?, a]]: Invariant[Schema.Collection[S, *]] =
      Invariant[[a] =>> Annotation[Base.CollectionBase[S, a]]].imapK([A] =>
        (annotation: Annotation[Base.CollectionBase[S, A]]) => Schema.Collection(annotation)
      )([A] => (schema: Schema.Collection[S, A]) => schema.self)

    given [S[a] <: Schema[?, a]]: Self.Collection[Schema.Collection, S] = Self
      .Collection[[s[a] <: S[a], a] =>> Annotation[Base.CollectionBase[s, a]], S]
      .imapK([s[a] <: S[a], a] => (annotation: Annotation[Base.CollectionBase[s, a]]) => Collection(annotation))(
        [s[a] <: S[a], a] => (schema: Collection[s, a]) => schema.self
      )

  sealed abstract class Constant[+S[a] <: Schema[?, a], A]
      extends Schema[S, A],
        Schema.Constant.Read[S, A],
        Schema.Constant.Write[S, A]:
    override def self: Annotation[Base.ConstantBase[S, A]]

  object Constant:
    sealed trait Read[+S[a] <: Schema.Read[?, a], +A] extends Schema.Read[S, A]:
      override def self: Annotation[Base.ConstantBase.Read[S, A]]

    object Read:
      def apply[S[a] <: Schema.Read[?, a], A](
          annotation: Annotation[Base.ConstantBase.Read[S, A]]
      ): Schema.Constant.Read[S, A] = new Read[S, A]:
        override def self: Annotation[Base.ConstantBase.Read[S, A]] = annotation

      def unapply[S[a] <: Schema.Read[?, a], A](
          schema: Schema.Constant.Read[S, A]
      ): Annotation[Base.ConstantBase.Read[S, A]] = schema.self

      given [S[a] <: Schema.Read[?, a], A]: Annotated[Schema.Constant.Read[S, A]] =
        Annotated[Annotation[Base.ConstantBase.Read[S, A]]].imap(Schema.Constant.Read.apply)(_.self)

      given [S[a] <: Schema.Read[?, a]]: Functor[Schema.Constant.Read[S, *]] =
        Functor[[a] =>> Annotation[Base.ConstantBase.Read[S, a]]].imapK([A] =>
          (annotation: Annotation[Base.ConstantBase.Read[S, A]]) => Schema.Constant.Read(annotation)
        )([A] => (schema: Schema.Constant.Read[S, A]) => schema.self)

      given [S[a] <: Schema.Read[?, a]]: Self.Constant.Read[Schema.Constant.Read, S] = Self.Constant
        .Read[[s[a] <: S[a], a] =>> Annotation[Base.ConstantBase.Read[s, a]], S]
        .imapK([s[a] <: S[a], a] => (annotation: Annotation[Base.ConstantBase.Read[s, a]]) => Read(annotation))(
          [s[a] <: S[a], a] => (schema: Schema.Constant.Read[s, a]) => schema.self
        )

    sealed trait Write[+S[a] <: Schema.Write[?, a], -A] extends Schema.Write[S, A]:
      override def self: Annotation[Base.ConstantBase.Write[S, A]]

    object Write:
      def apply[S[a] <: Schema.Write[?, a], A](
          annotation: Annotation[Base.ConstantBase.Write[S, A]]
      ): Schema.Constant.Write[S, A] = new Write[S, A]:
        override def self: Annotation[Base.ConstantBase.Write[S, A]] = annotation

      def unapply[S[a] <: Schema.Write[?, a], A](
          schema: Schema.Constant.Write[S, A]
      ): Annotation[Base.ConstantBase.Write[S, A]] = schema.self

      given [S[a] <: Schema.Write[?, a], A]: Annotated[Schema.Constant.Write[S, A]] =
        Annotated[Annotation[Base.ConstantBase.Write[S, A]]].imap(Schema.Constant.Write.apply)(_.self)

      given [S[a] <: Schema.Write[?, a]]: Contravariant[Schema.Constant.Write[S, *]] =
        Contravariant[[a] =>> Annotation[Base.ConstantBase.Write[S, a]]].imapK([A] =>
          (annotation: Annotation[Base.ConstantBase.Write[S, A]]) => Schema.Constant.Write(annotation)
        )([A] => (schema: Schema.Constant.Write[S, A]) => schema.self)

      given [S[a] <: Schema.Write[?, a]]: Self.Constant.Write[Schema.Constant.Write, S] = Self.Constant
        .Write[[s[a] <: S[a], a] =>> Annotation[Base.ConstantBase.Write[s, a]], S]
        .imapK([s[a] <: S[a], a] => (annotation: Annotation[Base.ConstantBase.Write[s, a]]) => Write(annotation))(
          [s[a] <: S[a], a] => (schema: Schema.Constant.Write[s, a]) => schema.self
        )

    def apply[S[a] <: Schema[?, a], A](annotation: Annotation[Base.ConstantBase[S, A]]): Schema.Constant[S, A] =
      new Constant[S, A]:
        override def self: Annotation[Base.ConstantBase[S, A]] = annotation

    def unapply[S[a] <: Schema[?, a], A](schema: Schema.Constant[S, A]): Annotation[Base.ConstantBase[S, A]] =
      schema.self

    given [S[a] <: Schema[?, a], A]: Annotated[Schema.Constant[S, A]] =
      Annotated[Annotation[Base.ConstantBase[S, A]]].imap(Schema.Constant.apply)(_.self)

    given [S[a] <: Schema[?, a]]: Invariant[Schema.Constant[S, *]] =
      Invariant[[a] =>> Annotation[Base.ConstantBase[S, a]]].imapK([A] =>
        (annotation: Annotation[Base.ConstantBase[S, A]]) => Schema.Constant(annotation)
      )([A] => (schema: Schema.Constant[S, A]) => schema.self)

    given [S[a] <: Schema[?, a]]: Self.Constant[Schema.Constant, S] = Self
      .Constant[[s[a] <: S[a], a] =>> Annotation[Base.ConstantBase[s, a]], S]
      .imapK([s[a] <: S[a], a] => (annotation: Annotation[Base.ConstantBase[s, a]]) => Constant(annotation))(
        [s[a] <: S[a], a] => (schema: Constant[s, a]) => schema.self
      )

  sealed abstract class Dictionary[+S[a] <: Schema[?, a], A]
      extends Schema[S, A],
        Schema.Dictionary.Read[S, A],
        Schema.Dictionary.Write[S, A]:
    override def self: Annotation[Base.DictionaryBase[S, A]]

  object Dictionary:
    sealed trait Read[+S[a] <: Schema.Read[?, a], +A] extends Schema.Read[S, A]:
      override def self: Annotation[Base.DictionaryBase.Read[S, A]]

    object Read:
      def apply[S[a] <: Schema.Read[?, a], A](
          annotation: Annotation[Base.DictionaryBase.Read[S, A]]
      ): Schema.Dictionary.Read[S, A] = new Read[S, A]:
        override def self: Annotation[Base.DictionaryBase.Read[S, A]] = annotation

      def unapply[S[a] <: Schema.Read[?, a], A](
          schema: Schema.Dictionary.Read[S, A]
      ): Annotation[Base.DictionaryBase.Read[S, A]] = schema.self

      given [S[a] <: Schema.Read[?, a], A]: Annotated[Schema.Dictionary.Read[S, A]] =
        Annotated[Annotation[Base.DictionaryBase.Read[S, A]]].imap(Schema.Dictionary.Read.apply)(_.self)

      given [S[a] <: Schema.Read[?, a]]: Functor[Schema.Dictionary.Read[S, *]] =
        Functor[[a] =>> Annotation[Base.DictionaryBase.Read[S, a]]].imapK([A] =>
          (annotation: Annotation[Base.DictionaryBase.Read[S, A]]) => Schema.Dictionary.Read(annotation)
        )([A] => (schema: Schema.Dictionary.Read[S, A]) => schema.self)

      given [S[a] <: Schema.Read[?, a]]: Self.Dictionary.Read[Schema.Dictionary.Read, S] = Self.Dictionary
        .Read[[s[a] <: S[a], a] =>> Annotation[Base.DictionaryBase.Read[s, a]], S]
        .imapK([s[a] <: S[a], a] => (annotation: Annotation[Base.DictionaryBase.Read[s, a]]) => Read(annotation))(
          [s[a] <: S[a], a] => (schema: Schema.Dictionary.Read[s, a]) => schema.self
        )

    sealed trait Write[+S[a] <: Schema.Write[?, a], -A] extends Schema.Write[S, A]:
      override def self: Annotation[Base.DictionaryBase.Write[S, A]]

    object Write:
      def apply[S[a] <: Schema.Write[?, a], A](
          annotation: Annotation[Base.DictionaryBase.Write[S, A]]
      ): Schema.Dictionary.Write[S, A] = new Write[S, A]:
        override def self: Annotation[Base.DictionaryBase.Write[S, A]] = annotation

      def unapply[S[a] <: Schema.Write[?, a], A](
          schema: Schema.Dictionary.Write[S, A]
      ): Annotation[Base.DictionaryBase.Write[S, A]] = schema.self

      given [S[a] <: Schema.Write[?, a], A]: Annotated[Schema.Dictionary.Write[S, A]] =
        Annotated[Annotation[Base.DictionaryBase.Write[S, A]]].imap(Schema.Dictionary.Write.apply)(_.self)

      given [S[a] <: Schema.Write[?, a]]: Contravariant[Schema.Dictionary.Write[S, *]] =
        Contravariant[[a] =>> Annotation[Base.DictionaryBase.Write[S, a]]].imapK([A] =>
          (annotation: Annotation[Base.DictionaryBase.Write[S, A]]) => Schema.Dictionary.Write(annotation)
        )([A] => (schema: Schema.Dictionary.Write[S, A]) => schema.self)

      given [S[a] <: Schema.Write[?, a]]: Self.Dictionary.Write[Schema.Dictionary.Write, S] = Self.Dictionary
        .Write[[s[a] <: S[a], a] =>> Annotation[Base.DictionaryBase.Write[s, a]], S]
        .imapK([s[a] <: S[a], a] => (annotation: Annotation[Base.DictionaryBase.Write[s, a]]) => Write(annotation))(
          [s[a] <: S[a], a] => (schema: Schema.Dictionary.Write[s, a]) => schema.self
        )

    def apply[S[a] <: Schema[?, a], A](annotation: Annotation[Base.DictionaryBase[S, A]]): Schema.Dictionary[S, A] =
      new Dictionary[S, A]:
        override def self: Annotation[Base.DictionaryBase[S, A]] = annotation

    def unapply[S[a] <: Schema[?, a], A](schema: Schema.Dictionary[S, A]): Annotation[Base.DictionaryBase[S, A]] =
      schema.self

    given [S[a] <: Schema[?, a], A]: Annotated[Schema.Dictionary[S, A]] =
      Annotated[Annotation[Base.DictionaryBase[S, A]]].imap(Schema.Dictionary.apply)(_.self)

    given [S[a] <: Schema[?, a]]: Invariant[Schema.Dictionary[S, *]] =
      Invariant[[a] =>> Annotation[Base.DictionaryBase[S, a]]].imapK([A] =>
        (annotation: Annotation[Base.DictionaryBase[S, A]]) => Schema.Dictionary(annotation)
      )([A] => (schema: Schema.Dictionary[S, A]) => schema.self)

    given [S[a] <: Schema[?, a]]: Self.Dictionary[Schema.Dictionary, S] = Self
      .Dictionary[[s[a] <: S[a], a] =>> Annotation[Base.DictionaryBase[s, a]], S]
      .imapK([s[a] <: S[a], a] => (annotation: Annotation[Base.DictionaryBase[s, a]]) => Dictionary(annotation))(
        [s[a] <: S[a], a] => (schema: Dictionary[s, a]) => schema.self
      )

  sealed abstract class Enumeration[+S[a] <: Schema[?, a], A]
      extends Schema[S, A],
        Schema.Enumeration.Read[S, A],
        Schema.Enumeration.Write[S, A]:
    override def self: Annotation[Base.EnumerationBase[S, A]]

  object Enumeration:
    sealed trait Read[+S[a] <: Schema.Read[?, a], +A] extends Schema.Read[S, A]:
      override def self: Annotation[Base.EnumerationBase.Read[S, A]]

    object Read:
      def apply[S[a] <: Schema.Read[?, a], A](
          annotation: Annotation[Base.EnumerationBase.Read[S, A]]
      ): Schema.Enumeration.Read[S, A] = new Read[S, A]:
        override def self: Annotation[Base.EnumerationBase.Read[S, A]] = annotation

      def unapply[S[a] <: Schema.Read[?, a], A](
          schema: Schema.Enumeration.Read[S, A]
      ): Annotation[Base.EnumerationBase.Read[S, A]] = schema.self

      given [S[a] <: Schema.Read[?, a], A]: Annotated[Schema.Enumeration.Read[S, A]] =
        Annotated[Annotation[Base.EnumerationBase.Read[S, A]]].imap(Schema.Enumeration.Read.apply)(_.self)

      given [S[a] <: Schema.Read[?, a]]: Functor[Schema.Enumeration.Read[S, *]] =
        Functor[[a] =>> Annotation[Base.EnumerationBase.Read[S, a]]].imapK([A] =>
          (annotation: Annotation[Base.EnumerationBase.Read[S, A]]) => Schema.Enumeration.Read(annotation)
        )([A] => (schema: Schema.Enumeration.Read[S, A]) => schema.self)

      given [S[a] <: Schema.Read[?, a]]: Self.Enumeration.Read[Schema.Enumeration.Read, S] = Self.Enumeration
        .Read[[s[a] <: S[a], a] =>> Annotation[Base.EnumerationBase.Read[s, a]], S]
        .imapK([s[a] <: S[a], a] => (annotation: Annotation[Base.EnumerationBase.Read[s, a]]) => Read(annotation))(
          [s[a] <: S[a], a] => (schema: Schema.Enumeration.Read[s, a]) => schema.self
        )

    sealed trait Write[+S[a] <: Schema.Write[?, a], -A] extends Schema.Write[S, A]:
      override def self: Annotation[Base.EnumerationBase.Write[S, A]]

    object Write:
      def apply[S[a] <: Schema.Write[?, a], A](
          annotation: Annotation[Base.EnumerationBase.Write[S, A]]
      ): Schema.Enumeration.Write[S, A] = new Write[S, A]:
        override def self: Annotation[Base.EnumerationBase.Write[S, A]] = annotation

      def unapply[S[a] <: Schema.Write[?, a], A](
          schema: Schema.Enumeration.Write[S, A]
      ): Annotation[Base.EnumerationBase.Write[S, A]] = schema.self

      given [S[a] <: Schema.Write[?, a], A]: Annotated[Schema.Enumeration.Write[S, A]] =
        Annotated[Annotation[Base.EnumerationBase.Write[S, A]]].imap(Schema.Enumeration.Write.apply)(_.self)

      given [S[a] <: Schema.Write[?, a]]: Contravariant[Schema.Enumeration.Write[S, *]] =
        Contravariant[[a] =>> Annotation[Base.EnumerationBase.Write[S, a]]].imapK([A] =>
          (annotation: Annotation[Base.EnumerationBase.Write[S, A]]) => Schema.Enumeration.Write(annotation)
        )([A] => (schema: Schema.Enumeration.Write[S, A]) => schema.self)

      given [S[a] <: Schema.Write[?, a]]: Self.Enumeration.Write[Schema.Enumeration.Write, S] = Self.Enumeration
        .Write[[s[a] <: S[a], a] =>> Annotation[Base.EnumerationBase.Write[s, a]], S]
        .imapK([s[a] <: S[a], a] => (annotation: Annotation[Base.EnumerationBase.Write[s, a]]) => Write(annotation))(
          [s[a] <: S[a], a] => (schema: Schema.Enumeration.Write[s, a]) => schema.self
        )

    def apply[S[a] <: Schema[?, a], A](annotation: Annotation[Base.EnumerationBase[S, A]]): Schema.Enumeration[S, A] =
      new Enumeration[S, A]:
        override def self: Annotation[Base.EnumerationBase[S, A]] = annotation

    def unapply[S[a] <: Schema[?, a], A](schema: Schema.Enumeration[S, A]): Annotation[Base.EnumerationBase[S, A]] =
      schema.self

    given [S[a] <: Schema[?, a], A]: Annotated[Schema.Enumeration[S, A]] =
      Annotated[Annotation[Base.EnumerationBase[S, A]]].imap(Schema.Enumeration.apply)(_.self)

    given [S[a] <: Schema[?, a]]: Invariant[Schema.Enumeration[S, *]] =
      Invariant[[a] =>> Annotation[Base.EnumerationBase[S, a]]].imapK([A] =>
        (annotation: Annotation[Base.EnumerationBase[S, A]]) => Schema.Enumeration(annotation)
      )([A] => (schema: Schema.Enumeration[S, A]) => schema.self)

    given [S[a] <: Schema[?, a]]: Self.Enumeration[Schema.Enumeration, S] = Self
      .Enumeration[[s[a] <: S[a], a] =>> Annotation[Base.EnumerationBase[s, a]], S]
      .imapK([s[a] <: S[a], a] => (annotation: Annotation[Base.EnumerationBase[s, a]]) => Enumeration(annotation))(
        [s[a] <: S[a], a] => (schema: Enumeration[s, a]) => schema.self
      )

  sealed abstract class Nullish[+S[a] <: Schema[?, a], A]
      extends Schema[S, A],
        Schema.Nullish.Read[S, A],
        Schema.Nullish.Write[S, A]:
    override def self: Annotation[Base.NullishBase[S, A]]

  object Nullish:
    sealed trait Read[+S[a] <: Schema.Read[?, a], +A] extends Schema.Read[S, A]:
      override def self: Annotation[Base.NullishBase.Read[S, A]]

    object Read:
      def apply[S[a] <: Schema.Read[?, a], A](
          annotation: Annotation[Base.NullishBase.Read[S, A]]
      ): Schema.Nullish.Read[S, A] = new Schema.Nullish.Read[S, A]:
        override def self: Annotation[Base.NullishBase.Read[S, A]] = annotation

      def unapply[S[a] <: Schema.Read[?, a], A](
          schema: Schema.Nullish.Read[S, A]
      ): Annotation[Base.NullishBase.Read[S, A]] = schema.self

      given [S[a] <: Schema.Read[?, a], A]: Annotated[Schema.Nullish.Read[S, A]] =
        Annotated[Annotation[Base.NullishBase.Read[S, A]]].imap(Schema.Nullish.Read.apply)(_.self)

      given [S[a] <: Schema.Read[?, a]]: Functor[Schema.Nullish.Read[S, *]] =
        Functor[[a] =>> Annotation[Base.NullishBase.Read[S, a]]].imapK([A] =>
          (annotation: Annotation[Base.NullishBase.Read[S, A]]) => Schema.Nullish.Read(annotation)
        )([A] => (schema: Schema.Nullish.Read[S, A]) => schema.self)

      given [S[a] <: Schema.Read[?, a]]: Self.Nullish.Read[Schema.Nullish.Read, S] = Self.Nullish
        .Read[[s[a] <: S[a], a] =>> Annotation[Base.NullishBase.Read[s, a]], S]
        .imapK([s[a] <: S[a], a] => (annotation: Annotation[Base.NullishBase.Read[s, a]]) => Read(annotation))(
          [s[a] <: S[a], a] => (schema: Schema.Nullish.Read[s, a]) => schema.self
        )

    sealed trait Write[+S[a] <: Schema.Write[?, a], -A] extends Schema.Write[S, A]:
      override def self: Annotation[Base.NullishBase.Write[S, A]]

    object Write:
      def apply[S[a] <: Schema.Write[?, a], A](
          annotation: Annotation[Base.NullishBase.Write[S, A]]
      ): Schema.Nullish.Write[S, A] = new Schema.Nullish.Write[S, A]:
        override def self: Annotation[Base.NullishBase.Write[S, A]] = annotation

      def unapply[S[a] <: Schema.Write[?, a], A](
          schema: Schema.Nullish.Write[S, A]
      ): Annotation[Base.NullishBase.Write[S, A]] = schema.self

      given [S[a] <: Schema.Write[?, a], A]: Annotated[Schema.Nullish.Write[S, A]] =
        Annotated[Annotation[Base.NullishBase.Write[S, A]]].imap(Schema.Nullish.Write.apply)(_.self)

      given [S[a] <: Schema.Write[?, a]]: Contravariant[Schema.Nullish.Write[S, *]] =
        Contravariant[[a] =>> Annotation[Base.NullishBase.Write[S, a]]].imapK([A] =>
          (annotation: Annotation[Base.NullishBase.Write[S, A]]) => Schema.Nullish.Write(annotation)
        )([A] => (schema: Schema.Nullish.Write[S, A]) => schema.self)

      given [S[a] <: Schema.Write[?, a]]: Self.Nullish.Write[Schema.Nullish.Write, S] = Self.Nullish
        .Write[[s[a] <: S[a], a] =>> Annotation[Base.NullishBase.Write[s, a]], S]
        .imapK([s[a] <: S[a], a] => (annotation: Annotation[Base.NullishBase.Write[s, a]]) => Write(annotation))(
          [s[a] <: S[a], a] => (schema: Schema.Nullish.Write[s, a]) => schema.self
        )

    def apply[S[a] <: Schema[?, a], A](annotation: Annotation[Base.NullishBase[S, A]]): Schema.Nullish[S, A] =
      new Nullish[S, A]:
        override def self: Annotation[Base.NullishBase[S, A]] = annotation

    def unapply[S[a] <: Schema[?, a], A](schema: Schema.Nullish[S, A]): Annotation[Base.NullishBase[S, A]] =
      schema.self

    given [S[a] <: Schema[?, a], A]: Annotated[Schema.Nullish[S, A]] =
      Annotated[Annotation[Base.NullishBase[S, A]]].imap(Schema.Nullish.apply)(_.self)

    given [S[a] <: Schema[?, a]]: Invariant[Schema.Nullish[S, *]] =
      Invariant[[a] =>> Annotation[Base.NullishBase[S, a]]].imapK([A] =>
        (annotation: Annotation[Base.NullishBase[S, A]]) => Schema.Nullish(annotation)
      )([A] => (schema: Schema.Nullish[S, A]) => schema.self)

    given [S[a] <: Schema[?, a]]: Self.Nullish[Schema.Nullish, S] = Self
      .Nullish[[s[a] <: S[a], a] =>> Annotation[Base.NullishBase[s, a]], S]
      .imapK([s[a] <: S[a], a] => (annotation: Annotation[Base.NullishBase[s, a]]) => Nullish(annotation))(
        [s[a] <: S[a], a] => (schema: Nullish[s, a]) => schema.self
      )

  sealed abstract class Record[+S[a] <: Schema[?, a], A]
      extends Schema[S, A],
        Schema.Record.Read[S, A],
        Schema.Record.Write[S, A]:
    override def self: Annotation[Base.RecordBase[S, A]]

  object Record:
    sealed trait Read[+S[a] <: Schema.Read[?, a], +A] extends Schema.Read[S, A]:
      override def self: Annotation[Base.RecordBase.Read[S, A]]

    object Read:
      def apply[S[a] <: Schema.Read[?, a], A](
          annotation: Annotation[Base.RecordBase.Read[S, A]]
      ): Schema.Record.Read[S, A] = new Schema.Record.Read[S, A]:
        override def self: Annotation[Base.RecordBase.Read[S, A]] = annotation

      def unapply[S[a] <: Schema.Read[?, a], A](
          schema: Schema.Record.Read[S, A]
      ): Annotation[Base.RecordBase.Read[S, A]] = schema.self

      given [S[a] <: Schema.Read[?, a], A]: Annotated[Schema.Record.Read[S, A]] =
        Annotated[Annotation[Base.RecordBase.Read[S, A]]].imap(Schema.Record.Read.apply)(_.self)

      given [S[a] <: Schema.Read[?, a]]: Functor[Schema.Record.Read[S, *]] =
        Functor[[a] =>> Annotation[Base.RecordBase.Read[S, a]]].imapK([A] =>
          (annotation: Annotation[Base.RecordBase.Read[S, A]]) => Schema.Record.Read(annotation)
        )([A] => (schema: Schema.Record.Read[S, A]) => schema.self)

      given [S[a] <: Schema.Read[?, a]]: Self.Record.Read[Schema.Record.Read, S] = Self.Record
        .Read[[s[a] <: S[a], a] =>> Annotation[Base.RecordBase.Read[s, a]], S]
        .imapK([s[a] <: S[a], a] => (annotation: Annotation[Base.RecordBase.Read[s, a]]) => Read(annotation))(
          [s[a] <: S[a], a] => (schema: Schema.Record.Read[s, a]) => schema.self
        )

    sealed trait Write[+S[a] <: Schema.Write[?, a], -A] extends Schema.Write[S, A]:
      override def self: Annotation[Base.RecordBase.Write[S, A]]

    object Write:
      def apply[S[a] <: Schema.Write[?, a], A](
          annotation: Annotation[Base.RecordBase.Write[S, A]]
      ): Schema.Record.Write[S, A] = new Schema.Record.Write[S, A]:
        override def self: Annotation[Base.RecordBase.Write[S, A]] = annotation

      def unapply[S[a] <: Schema.Write[?, a], A](
          schema: Schema.Record.Write[S, A]
      ): Annotation[Base.RecordBase.Write[S, A]] = schema.self

      given [S[a] <: Schema.Write[?, a], A]: Annotated[Schema.Record.Write[S, A]] =
        Annotated[Annotation[Base.RecordBase.Write[S, A]]].imap(Schema.Record.Write.apply)(_.self)

      given [S[a] <: Schema.Write[?, a]]: Contravariant[Schema.Record.Write[S, *]] =
        Contravariant[[a] =>> Annotation[Base.RecordBase.Write[S, a]]].imapK([A] =>
          (annotation: Annotation[Base.RecordBase.Write[S, A]]) => Schema.Record.Write(annotation)
        )([A] => (schema: Schema.Record.Write[S, A]) => schema.self)

      given [S[a] <: Schema.Write[?, a]]: Self.Record.Write[Schema.Record.Write, S] = Self.Record
        .Write[[s[a] <: S[a], a] =>> Annotation[Base.RecordBase.Write[s, a]], S]
        .imapK([s[a] <: S[a], a] => (annotation: Annotation[Base.RecordBase.Write[s, a]]) => Write(annotation))(
          [s[a] <: S[a], a] => (schema: Schema.Record.Write[s, a]) => schema.self
        )

    def apply[S[a] <: Schema[?, a], A](annotation: Annotation[Base.RecordBase[S, A]]): Schema.Record[S, A] =
      new Record[S, A]:
        override def self: Annotation[Base.RecordBase[S, A]] = annotation

    def unapply[S[a] <: Schema[?, a], A](schema: Schema.Record[S, A]): Annotation[Base.RecordBase[S, A]] =
      schema.self

    given [S[a] <: Schema[?, a], A]: Annotated[Schema.Record[S, A]] =
      Annotated[Annotation[Base.RecordBase[S, A]]].imap(Schema.Record.apply)(_.self)

    given [S[a] <: Schema[?, a]]: Invariant[Schema.Record[S, *]] =
      Invariant[[a] =>> Annotation[Base.RecordBase[S, a]]].imapK([A] =>
        (annotation: Annotation[Base.RecordBase[S, A]]) => Schema.Record(annotation)
      )([A] => (schema: Schema.Record[S, A]) => schema.self)

    given [S[a] <: Schema[?, a]]: Self.Record[Schema.Record, S] = Self
      .Record[[s[a] <: S[a], a] =>> Annotation[Base.RecordBase[s, a]], S]
      .imapK([s[a] <: S[a], a] => (annotation: Annotation[Base.RecordBase[s, a]]) => Record(annotation))(
        [s[a] <: S[a], a] => (schema: Record[s, a]) => schema.self
      )

  sealed abstract class Tuple[+S[a] <: Schema[?, a], A]
      extends Schema[S, A],
        Schema.Tuple.Read[S, A],
        Schema.Tuple.Write[S, A]:
    override def self: Annotation[Base.TupleBase[S, A]]

  object Tuple:
    sealed trait Read[+S[a] <: Schema.Read[?, a], +A] extends Schema.Read[S, A]:
      override def self: Annotation[Base.TupleBase.Read[S, A]]

    object Read:
      def apply[S[a] <: Schema.Read[?, a], A](
          annotation: Annotation[Base.TupleBase.Read[S, A]]
      ): Schema.Tuple.Read[S, A] = new Schema.Tuple.Read[S, A]:
        override def self: Annotation[Base.TupleBase.Read[S, A]] = annotation

      def unapply[S[a] <: Schema.Read[?, a], A](
          schema: Schema.Tuple.Read[S, A]
      ): Annotation[Base.TupleBase.Read[S, A]] = schema.self

      given [S[a] <: Schema.Read[?, a], A]: Annotated[Schema.Tuple.Read[S, A]] =
        Annotated[Annotation[Base.TupleBase.Read[S, A]]].imap(Schema.Tuple.Read.apply)(_.self)

      given [S[a] <: Schema.Read[?, a]]: Functor[Schema.Tuple.Read[S, *]] =
        Functor[[a] =>> Annotation[Base.TupleBase.Read[S, a]]].imapK([A] =>
          (annotation: Annotation[Base.TupleBase.Read[S, A]]) => Schema.Tuple.Read(annotation)
        )([A] => (schema: Schema.Tuple.Read[S, A]) => schema.self)

      given [S[a] <: Schema.Read[?, a]]: Self.Tuple.Read[Schema.Tuple.Read, S] = Self.Tuple
        .Read[[s[a] <: S[a], a] =>> Annotation[Base.TupleBase.Read[s, a]], S]
        .imapK([s[a] <: S[a], a] => (annotation: Annotation[Base.TupleBase.Read[s, a]]) => Read(annotation))(
          [s[a] <: S[a], a] => (schema: Schema.Tuple.Read[s, a]) => schema.self
        )

    sealed trait Write[+S[a] <: Schema.Write[?, a], -A] extends Schema.Write[S, A]:
      override def self: Annotation[Base.TupleBase.Write[S, A]]

    object Write:
      def apply[S[a] <: Schema.Write[?, a], A](
          annotation: Annotation[Base.TupleBase.Write[S, A]]
      ): Schema.Tuple.Write[S, A] = new Schema.Tuple.Write[S, A]:
        override def self: Annotation[Base.TupleBase.Write[S, A]] = annotation

      def unapply[S[a] <: Schema.Write[?, a], A](
          schema: Schema.Tuple.Write[S, A]
      ): Annotation[Base.TupleBase.Write[S, A]] = schema.self

      given [S[a] <: Schema.Write[?, a], A]: Annotated[Schema.Tuple.Write[S, A]] =
        Annotated[Annotation[Base.TupleBase.Write[S, A]]].imap(Schema.Tuple.Write.apply)(_.self)

      given [S[a] <: Schema.Write[?, a]]: Contravariant[Schema.Tuple.Write[S, *]] =
        Contravariant[[a] =>> Annotation[Base.TupleBase.Write[S, a]]].imapK([A] =>
          (annotation: Annotation[Base.TupleBase.Write[S, A]]) => Schema.Tuple.Write(annotation)
        )([A] => (schema: Schema.Tuple.Write[S, A]) => schema.self)

      given [S[a] <: Schema.Write[?, a]]: Self.Tuple.Write[Schema.Tuple.Write, S] = Self.Tuple
        .Write[[s[a] <: S[a], a] =>> Annotation[Base.TupleBase.Write[s, a]], S]
        .imapK([s[a] <: S[a], a] => (annotation: Annotation[Base.TupleBase.Write[s, a]]) => Write(annotation))(
          [s[a] <: S[a], a] => (schema: Schema.Tuple.Write[s, a]) => schema.self
        )

    def apply[S[a] <: Schema[?, a], A](annotation: Annotation[Base.TupleBase[S, A]]): Schema.Tuple[S, A] =
      new Tuple[S, A]:
        override def self: Annotation[Base.TupleBase[S, A]] = annotation

    def unapply[S[a] <: Schema[?, a], A](schema: Schema.Tuple[S, A]): Annotation[Base.TupleBase[S, A]] =
      schema.self

    given [S[a] <: Schema[?, a], A]: Annotated[Schema.Tuple[S, A]] =
      Annotated[Annotation[Base.TupleBase[S, A]]].imap(Schema.Tuple.apply)(_.self)

    given [S[a] <: Schema[?, a]]: Invariant[Schema.Tuple[S, *]] =
      Invariant[[a] =>> Annotation[Base.TupleBase[S, a]]].imapK([A] =>
        (annotation: Annotation[Base.TupleBase[S, A]]) => Schema.Tuple(annotation)
      )([A] => (schema: Schema.Tuple[S, A]) => schema.self)

    given [S[a] <: Schema[?, a]]: Self.Tuple[Schema.Tuple, S] = Self
      .Tuple[[s[a] <: S[a], a] =>> Annotation[Base.TupleBase[s, a]], S]
      .imapK([s[a] <: S[a], a] => (annotation: Annotation[Base.TupleBase[s, a]]) => Tuple(annotation))(
        [s[a] <: S[a], a] => (schema: Tuple[s, a]) => schema.self
      )

  sealed abstract class Primitive[A] extends Schema[Nothing, A], Schema.Primitive.Read[A], Schema.Primitive.Write[A]:
    override def self: Annotation[Base.PrimitiveBase[A]]

  object Primitive:
    sealed trait Read[+A] extends Schema.Read[Nothing, A]:
      override def self: Annotation[Base.PrimitiveBase.Read[A]]

    object Read:
      def apply[A](annotation: Annotation[Base.PrimitiveBase.Read[A]]): Schema.Primitive.Read[A] = annotation.self match
        case self: Base.PrimitiveBase.Boolean.Read[A] => Boolean.Read(annotation.copy(self = self))
        case self: Base.PrimitiveBase.Number.Read[A]  => Number.Read(annotation.copy(self = self))
        case self: Base.PrimitiveBase.Text.Read[A]    => Text.Read(annotation.copy(self = self))

      def unapply[A](schema: Schema.Primitive.Read[A]): Annotation[Base.PrimitiveBase.Read[A]] = schema.self

      given [A]: Annotated[Schema.Primitive.Read[A]] =
        Annotated[Annotation[Base.PrimitiveBase.Read[A]]].imap(Schema.Primitive.Read.apply)(_.self)

    sealed trait Write[-A] extends Schema.Write[Nothing, A]:
      override def self: Annotation[Base.PrimitiveBase.Write[A]]

    object Write:
      def apply[A](annotation: Annotation[Base.PrimitiveBase.Write[A]]): Schema.Primitive.Write[A] =
        annotation.self match
          case self: Base.PrimitiveBase.Boolean.Write[A] => Boolean.Write(annotation.copy(self = self))
          case self: Base.PrimitiveBase.Number.Write[A]  => Number.Write(annotation.copy(self = self))
          case self: Base.PrimitiveBase.Text.Write[A]    => Text.Write(annotation.copy(self = self))

      def unapply[A](schema: Schema.Primitive.Write[A]): Annotation[Base.PrimitiveBase.Write[A]] = schema.self

      given [A]: Annotated[Schema.Primitive.Write[A]] = Annotated[Annotation[Base.PrimitiveBase.Write[A]]]
        .imap(Schema.Primitive.Write.apply)(_.self)

    sealed abstract class Boolean[A]
        extends Schema.Primitive[A],
          Schema.Primitive.Boolean.Read[A],
          Schema.Primitive.Boolean.Write[A]:
      override def self: Annotation[Base.PrimitiveBase.Boolean[A]]

    object Boolean:
      sealed trait Read[+A] extends Schema.Primitive.Read[A]:
        override def self: Annotation[Base.PrimitiveBase.Boolean.Read[A]]

      object Read:
        def apply[A](annotation: Annotation[Base.PrimitiveBase.Boolean.Read[A]]): Schema.Primitive.Boolean.Read[A] =
          new Schema.Primitive.Boolean.Read[A]:
            override def self: Annotation[Base.PrimitiveBase.Boolean.Read[A]] = annotation

        def unapply[A](schema: Schema.Primitive.Boolean.Read[A]): Annotation[Base.PrimitiveBase.Boolean.Read[A]] =
          schema.self

        given [A]: Annotated[Schema.Primitive.Boolean.Read[A]] =
          Annotated[Annotation[Base.PrimitiveBase.Boolean.Read[A]]].imap(Schema.Primitive.Boolean.Read.apply)(_.self)

        given Functor[Schema.Primitive.Boolean.Read] =
          Functor[[a] =>> Annotation[Base.PrimitiveBase.Boolean.Read[a]]].imapK([A] =>
            (annotation: Annotation[Base.PrimitiveBase.Boolean.Read[A]]) => Schema.Primitive.Boolean.Read(annotation)
          )([A] => (schema: Schema.Primitive.Boolean.Read[A]) => schema.self)

      sealed trait Write[-A] extends Schema.Primitive.Write[A]:
        override def self: Annotation[Base.PrimitiveBase.Boolean.Write[A]]

      object Write:
        def apply[A](annotation: Annotation[Base.PrimitiveBase.Boolean.Write[A]]): Schema.Primitive.Boolean.Write[A] =
          new Schema.Primitive.Boolean.Write[A]:
            override def self: Annotation[Base.PrimitiveBase.Boolean.Write[A]] = annotation

        def unapply[A](schema: Schema.Primitive.Boolean.Write[A]): Annotation[Base.PrimitiveBase.Boolean.Write[A]] =
          schema.self

        given [A]: Annotated[Schema.Primitive.Boolean.Write[A]] =
          Annotated[Annotation[Base.PrimitiveBase.Boolean.Write[A]]].imap(Schema.Primitive.Boolean.Write.apply)(_.self)

        given Contravariant[Schema.Primitive.Boolean.Write] =
          Contravariant[[a] =>> Annotation[Base.PrimitiveBase.Boolean.Write[a]]].imapK([A] =>
            (annotation: Annotation[Base.PrimitiveBase.Boolean.Write[A]]) => Schema.Primitive.Boolean.Write(annotation)
          )([A] => (schema: Schema.Primitive.Boolean.Write[A]) => schema.self)

      def apply[A](annotation: Annotation[Base.PrimitiveBase.Boolean[A]]): Schema.Primitive.Boolean[A] = new Boolean[A]:
        override def self: Annotation[Base.PrimitiveBase.Boolean[A]] = annotation

      def unapply[A](schema: Schema.Primitive.Boolean[A]): Annotation[Base.PrimitiveBase.Boolean[A]] = schema.self

      given [A]: Annotated[Schema.Primitive.Boolean[A]] =
        Annotated[Annotation[Base.PrimitiveBase.Boolean[A]]].imap(Schema.Primitive.Boolean.apply)(_.self)

      given Invariant[Schema.Primitive.Boolean] =
        Invariant[[a] =>> Annotation[Base.PrimitiveBase.Boolean[a]]].imapK([A] =>
          (annotation: Annotation[Base.PrimitiveBase.Boolean[A]]) => Schema.Primitive.Boolean(annotation)
        )([A] => (schema: Schema.Primitive.Boolean[A]) => schema.self)

    sealed abstract class Number[A]
        extends Schema.Primitive[A],
          Schema.Primitive.Number.Read[A],
          Schema.Primitive.Number.Write[A]:
      override def self: Annotation[Base.PrimitiveBase.Number[A]]

    object Number:
      sealed trait Read[+A] extends Schema.Primitive.Read[A]:
        override def self: Annotation[Base.PrimitiveBase.Number.Read[A]]

      object Read:
        def apply[A](annotation: Annotation[Base.PrimitiveBase.Number.Read[A]]): Schema.Primitive.Number.Read[A] =
          new Schema.Primitive.Number.Read[A]:
            override def self: Annotation[Base.PrimitiveBase.Number.Read[A]] = annotation

        def unapply[A](schema: Schema.Primitive.Number.Read[A]): Annotation[Base.PrimitiveBase.Number.Read[A]] =
          schema.self

        given [A]: Annotated[Schema.Primitive.Number.Read[A]] =
          Annotated[Annotation[Base.PrimitiveBase.Number.Read[A]]].imap(Schema.Primitive.Number.Read.apply)(_.self)

        given Functor[Schema.Primitive.Number.Read] =
          Functor[[a] =>> Annotation[Base.PrimitiveBase.Number.Read[a]]].imapK([A] =>
            (annotation: Annotation[Base.PrimitiveBase.Number.Read[A]]) => Schema.Primitive.Number.Read(annotation)
          )([A] => (schema: Schema.Primitive.Number.Read[A]) => schema.self)

      sealed trait Write[-A] extends Schema.Primitive.Write[A]:
        override def self: Annotation[Base.PrimitiveBase.Number.Write[A]]

      object Write:
        def apply[A](annotation: Annotation[Base.PrimitiveBase.Number.Write[A]]): Schema.Primitive.Number.Write[A] =
          new Schema.Primitive.Number.Write[A]:
            override def self: Annotation[Base.PrimitiveBase.Number.Write[A]] = annotation

        def unapply[A](schema: Schema.Primitive.Number.Write[A]): Annotation[Base.PrimitiveBase.Number.Write[A]] =
          schema.self

        given [A]: Annotated[Schema.Primitive.Number.Write[A]] =
          Annotated[Annotation[Base.PrimitiveBase.Number.Write[A]]].imap(Schema.Primitive.Number.Write.apply)(_.self)

        given Contravariant[Schema.Primitive.Number.Write] =
          Contravariant[[a] =>> Annotation[Base.PrimitiveBase.Number.Write[a]]].imapK([A] =>
            (annotation: Annotation[Base.PrimitiveBase.Number.Write[A]]) => Schema.Primitive.Number.Write(annotation)
          )([A] => (schema: Schema.Primitive.Number.Write[A]) => schema.self)

      def apply[A](annotation: Annotation[Base.PrimitiveBase.Number[A]]): Schema.Primitive.Number[A] = new Number[A]:
        override def self: Annotation[Base.PrimitiveBase.Number[A]] = annotation

      def unapply[A](schema: Schema.Primitive.Number[A]): Annotation[Base.PrimitiveBase.Number[A]] = schema.self

      given [A]: Annotated[Schema.Primitive.Number[A]] =
        Annotated[Annotation[Base.PrimitiveBase.Number[A]]].imap(Schema.Primitive.Number.apply)(_.self)

      given Invariant[Schema.Primitive.Number] =
        Invariant[[a] =>> Annotation[Base.PrimitiveBase.Number[a]]].imapK([A] =>
          (annotation: Annotation[Base.PrimitiveBase.Number[A]]) => Schema.Primitive.Number(annotation)
        )([A] => (schema: Schema.Primitive.Number[A]) => schema.self)

    sealed abstract class Text[A]
        extends Schema.Primitive[A],
          Schema.Primitive.Text.Read[A],
          Schema.Primitive.Text.Write[A]:
      override def self: Annotation[Base.PrimitiveBase.Text[A]]

    object Text:
      sealed trait Read[+A] extends Schema.Primitive.Read[A]:
        override def self: Annotation[Base.PrimitiveBase.Text.Read[A]]

      object Read:
        def apply[A](annotation: Annotation[Base.PrimitiveBase.Text.Read[A]]): Schema.Primitive.Text.Read[A] =
          new Schema.Primitive.Text.Read[A]:
            override def self: Annotation[Base.PrimitiveBase.Text.Read[A]] = annotation

        def unapply[A](schema: Schema.Primitive.Text.Read[A]): Annotation[Base.PrimitiveBase.Text.Read[A]] = schema.self

        given [A]: Annotated[Schema.Primitive.Text.Read[A]] =
          Annotated[Annotation[Base.PrimitiveBase.Text.Read[A]]].imap(Schema.Primitive.Text.Read.apply)(_.self)

        given Functor[Schema.Primitive.Text.Read] =
          Functor[[a] =>> Annotation[Base.PrimitiveBase.Text.Read[a]]].imapK([A] =>
            (annotation: Annotation[Base.PrimitiveBase.Text.Read[A]]) => Schema.Primitive.Text.Read(annotation)
          )([A] => (schema: Schema.Primitive.Text.Read[A]) => schema.self)

      sealed trait Write[-A] extends Schema.Primitive.Write[A]:
        override def self: Annotation[Base.PrimitiveBase.Text.Write[A]]

      object Write:
        def apply[A](annotation: Annotation[Base.PrimitiveBase.Text.Write[A]]): Schema.Primitive.Text.Write[A] =
          new Schema.Primitive.Text.Write[A]:
            override def self: Annotation[Base.PrimitiveBase.Text.Write[A]] = annotation

        def unapply[A](schema: Schema.Primitive.Text.Write[A]): Annotation[Base.PrimitiveBase.Text.Write[A]] =
          schema.self

        given [A]: Annotated[Schema.Primitive.Text.Write[A]] =
          Annotated[Annotation[Base.PrimitiveBase.Text.Write[A]]].imap(Schema.Primitive.Text.Write.apply)(_.self)

        given Contravariant[Schema.Primitive.Text.Write] =
          Contravariant[[a] =>> Annotation[Base.PrimitiveBase.Text.Write[a]]].imapK([A] =>
            (annotation: Annotation[Base.PrimitiveBase.Text.Write[A]]) => Schema.Primitive.Text.Write(annotation)
          )([A] => (schema: Schema.Primitive.Text.Write[A]) => schema.self)

      def apply[A](annotation: Annotation[Base.PrimitiveBase.Text[A]]): Schema.Primitive.Text[A] =
        new Schema.Primitive.Text[A]:
          override def self: Annotation[Base.PrimitiveBase.Text[A]] = annotation

      def unapply[A](schema: Schema.Primitive.Text[A]): Annotation[Base.PrimitiveBase.Text[A]] = schema.self

      given [A]: Annotated[Schema.Primitive.Text[A]] =
        Annotated[Annotation[Base.PrimitiveBase.Text[A]]].imap(Schema.Primitive.Text.apply)(_.self)

      given Invariant[Schema.Primitive.Text] =
        Invariant[[a] =>> Annotation[Base.PrimitiveBase.Text[a]]].imapK([A] =>
          (annotation: Annotation[Base.PrimitiveBase.Text[A]]) => Schema.Primitive.Text(annotation)
        )([A] => (schema: Schema.Primitive.Text[A]) => schema.self)

    def apply[A](annotation: Annotation[Base.PrimitiveBase[A]]): Schema.Primitive[A] = annotation.self match
      case self: Base.PrimitiveBase.Boolean[A] => Schema.Primitive.Boolean(annotation.copy(self = self))
      case self: Base.PrimitiveBase.Number[A]  => Schema.Primitive.Number(annotation.copy(self = self))
      case self: Base.PrimitiveBase.Text[A]    => Schema.Primitive.Text(annotation.copy(self = self))

    def unapply[A](schema: Schema.Primitive[A]): Annotation[Base.PrimitiveBase[A]] = schema.self

    given [A]: Annotated[Schema.Primitive[A]] =
      Annotated[Annotation[Base.PrimitiveBase[A]]].imap(Schema.Primitive.apply)(_.self)

  sealed abstract class Union[+S[a] <: Schema[?, a], A]
      extends Schema[S, A],
        Schema.Union.Read[S, A],
        Schema.Union.Write[S, A]:
    override def self: Annotation[Base.UnionBase[S, A]]

  object Union:
    sealed trait Read[+S[a] <: Schema.Read[?, a], +A] extends Schema.Read[S, A]:
      override def self: Annotation[Base.UnionBase.Read[S, A]]

    object Read:
      def apply[S[a] <: Schema.Read[?, a], A](
          annotation: Annotation[Base.UnionBase.Read[S, A]]
      ): Schema.Union.Read[S, A] = new Schema.Union.Read[S, A]:
        override def self: Annotation[Base.UnionBase.Read[S, A]] = annotation

      def unapply[S[a] <: Schema.Read[?, a], A](
          schema: Schema.Union.Read[S, A]
      ): Annotation[Base.UnionBase.Read[S, A]] = schema.self

      given [S[a] <: Schema.Read[?, a], A]: Annotated[Schema.Union.Read[S, A]] =
        Annotated[Annotation[Base.UnionBase.Read[S, A]]].imap(Schema.Union.Read.apply)(_.self)

      given [S[a] <: Schema.Read[?, a]]: Functor[Schema.Union.Read[S, *]] =
        Functor[[a] =>> Annotation[Base.UnionBase.Read[S, a]]].imapK([A] =>
          (annotation: Annotation[Base.UnionBase.Read[S, A]]) => Schema.Union.Read(annotation)
        )([A] => (schema: Schema.Union.Read[S, A]) => schema.self)

      given [S[a] <: Schema.Read[?, a]]: Self.Union.Read[Schema.Union.Read, S] = Self.Union
        .Read[[s[a] <: S[a], a] =>> Annotation[Base.UnionBase.Read[s, a]], S]
        .imapK([s[a] <: S[a], a] => (annotation: Annotation[Base.UnionBase.Read[s, a]]) => Read(annotation))(
          [s[a] <: S[a], a] => (schema: Schema.Union.Read[s, a]) => schema.self
        )

    sealed trait Write[+S[a] <: Schema.Write[?, a], -A] extends Schema.Write[S, A]:
      override def self: Annotation[Base.UnionBase.Write[S, A]]

    object Write:
      def apply[S[a] <: Schema.Write[?, a], A](
          annotation: Annotation[Base.UnionBase.Write[S, A]]
      ): Schema.Union.Write[S, A] = new Schema.Union.Write[S, A]:
        override def self: Annotation[Base.UnionBase.Write[S, A]] = annotation

      def unapply[S[a] <: Schema.Write[?, a], A](
          schema: Schema.Union.Write[S, A]
      ): Annotation[Base.UnionBase.Write[S, A]] = schema.self

      given [S[a] <: Schema.Write[?, a], A]: Annotated[Schema.Union.Write[S, A]] =
        Annotated[Annotation[Base.UnionBase.Write[S, A]]].imap(Schema.Union.Write.apply)(_.self)

      given [S[a] <: Schema.Write[?, a]]: Contravariant[Schema.Union.Write[S, *]] =
        Contravariant[[a] =>> Annotation[Base.UnionBase.Write[S, a]]].imapK([A] =>
          (annotation: Annotation[Base.UnionBase.Write[S, A]]) => Schema.Union.Write(annotation)
        )([A] => (schema: Schema.Union.Write[S, A]) => schema.self)

      given [S[a] <: Schema.Write[?, a]]: Self.Union.Write[Schema.Union.Write, S] = Self.Union
        .Write[[s[a] <: S[a], a] =>> Annotation[Base.UnionBase.Write[s, a]], S]
        .imapK([s[a] <: S[a], a] => (annotation: Annotation[Base.UnionBase.Write[s, a]]) => Write(annotation))(
          [s[a] <: S[a], a] => (schema: Schema.Union.Write[s, a]) => schema.self
        )

    def apply[S[a] <: Schema[?, a], A](annotation: Annotation[Base.UnionBase[S, A]]): Schema.Union[S, A] =
      new Union[S, A]:
        override def self: Annotation[Base.UnionBase[S, A]] = annotation

    def unapply[S[a] <: Schema[?, a], A](schema: Schema.Union[S, A]): Annotation[Base.UnionBase[S, A]] =
      schema.self

    given [S[a] <: Schema[?, a], A]: Annotated[Schema.Union[S, A]] =
      Annotated[Annotation[Base.UnionBase[S, A]]].imap(Schema.Union.apply)(_.self)

    given [S[a] <: Schema[?, a]]: Invariant[Schema.Union[S, *]] =
      Invariant[[a] =>> Annotation[Base.UnionBase[S, a]]].imapK([A] =>
        (annotation: Annotation[Base.UnionBase[S, A]]) => Schema.Union(annotation)
      )([A] => (schema: Schema.Union[S, A]) => schema.self)

    given [S[a] <: Schema[?, a]]: Self.Union[Schema.Union, S] = Self
      .Union[[s[a] <: S[a], a] =>> Annotation[Base.UnionBase[s, a]], S]
      .imapK([s[a] <: S[a], a] => (annotation: Annotation[Base.UnionBase[s, a]]) => Union(annotation))(
        [s[a] <: S[a], a] => (schema: Union[s, a]) => schema.self
      )

  def apply[S[a] <: Schema[?, a], A](annotation: Annotation[Schema.Of[S, A]]): Schema[S, A] = annotation.self match
    case self: Base.CoerceBase[S, A]      => Schema.Coerce(annotation.copy(self = self))
    case self: Base.CollectionBase[S, A]  => Schema.Collection(annotation.copy(self = self))
    case self: Base.ConstantBase[S, A]    => Schema.Constant(annotation.copy(self = self))
    case self: Base.DictionaryBase[S, A]  => Schema.Dictionary(annotation.copy(self = self))
    case self: Base.EnumerationBase[S, A] => Schema.Enumeration(annotation.copy(self = self))
    case self: Base.NullishBase[S, A]     => Schema.Nullish(annotation.copy(self = self))
    case self: Base.PrimitiveBase[A]      => Schema.Primitive(annotation.copy(self = self))
    case self: Base.RecordBase[S, A]      => Schema.Record(annotation.copy(self = self))
    case self: Base.TupleBase[S, A]       => Schema.Tuple(annotation.copy(self = self))
    case self: Base.UnionBase[S, A]       => Schema.Union(annotation.copy(self = self))

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
