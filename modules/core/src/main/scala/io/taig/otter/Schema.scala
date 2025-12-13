package io.taig.otter

import cats.Contravariant
import cats.Functor
import cats.Invariant
import cats.syntax.all.*
import io.taig.otter as Self
import io.taig.otter.base.*
import io.taig.otter.syntax.CatsSyntax.*

sealed abstract class Schema[+S[a] <: Schema[?, a], A] extends Schema.Read[S, A], Schema.Write[S, A]:
  override def self: Annotation[Schema.Of[S, A]]

object Schema:
  type Of[+S[a] <: Schema[?, a], A] = CoerceBase[S, A] | CollectionBase[S, A] | ConstantBase[S, A] |
    DictionaryBase[S, A] | EnumerationBase[S, A] | NullishBase[S, A] | PrimitiveBase[A] |
    RecordBase[Schema.Field[S, *], A] | TupleBase[S, A] | UnionBase[Schema.Branch[S, *], A]

  sealed trait Read[+S[a] <: Schema.Read[?, a], +A]:
    def self: Annotation[Schema.Read.Of[S, A]]

  object Read:
    type Of[+S[a] <: Schema.Read[?, a], +A] = CoerceBase.Read[S, A] | CollectionBase.Read[S, A] |
      ConstantBase.Read[S, A] | DictionaryBase.Read[S, A] | EnumerationBase.Read[S, A] | NullishBase.Read[S, A] |
      PrimitiveBase.Read[A] | RecordBase.Read[Schema.Field.Read[S, *], A] | TupleBase.Read[S, A] |
      UnionBase.Read[Schema.Branch.Read[S, *], A]

    def apply[S[a] <: Schema.Read[?, a], A](annotation: Annotation[Schema.Read.Of[S, A]]): Schema.Read[S, A] =
      annotation.self match
        case self: CoerceBase.Read[S, A]                       => Schema.Coerce.Read(annotation.copy(self = self))
        case self: CollectionBase.Read[S, A]                   => Schema.Collection.Read(annotation.copy(self = self))
        case self: ConstantBase.Read[S, A]                     => Schema.Constant.Read(annotation.copy(self = self))
        case self: DictionaryBase.Read[S, A]                   => Schema.Dictionary.Read(annotation.copy(self = self))
        case self: EnumerationBase.Read[S, A]                  => Schema.Enumeration.Read(annotation.copy(self = self))
        case self: NullishBase.Read[S, A]                      => Schema.Nullish.Read(annotation.copy(self = self))
        case self: PrimitiveBase.Read[A]                       => Schema.Primitive.Read(annotation.copy(self = self))
        case self: RecordBase.Read[Schema.Field.Read[S, *], A] => Schema.Record.Read(annotation.copy(self = self))
        case self: TupleBase.Read[S, A]                        => Schema.Tuple.Read(annotation.copy(self = self))
        case self: UnionBase.Read[Schema.Branch.Read[S, *], A] => Schema.Union.Read(annotation.copy(self = self))

    def unapply[S[a] <: Schema.Read[?, a], A](schema: Schema.Read[S, A]): Annotation[Schema.Read.Of[S, A]] = schema.self

    given [S[a] <: Schema.Read[?, a], A]: Annotated[Schema.Read[S, A]] =
      Annotated[Annotation[Schema.Read.Of[S, A]]].imap(Schema.Read.apply)(_.self)

    given [S[a] <: Schema.Read[?, a]]: Functor[Schema.Read[S, *]] with
      override def map[A, B](fa: Schema.Read[S, A])(f: A => B): Schema.Read[S, B] = fa match
        case schema: Schema.Coerce.Read[S, A]      => schema.map(f)
        case schema: Schema.Collection.Read[S, A]  => schema.map(f)
        case schema: Schema.Constant.Read[S, A]    => schema.map(f)
        case schema: Schema.Dictionary.Read[S, A]  => schema.map(f)
        case schema: Schema.Enumeration.Read[S, A] => schema.map(f)
        case schema: Schema.Nullish.Read[S, A]     => schema.map(f)
        case schema: Schema.Primitive.Read[A]      => schema.map(f)
        case schema: Schema.Record.Read[S, A]      => schema.map(f)
        case schema: Schema.Tuple.Read[S, A]       => schema.map(f)
        case schema: Schema.Union.Read[S, A]       => schema.map(f)

  sealed trait Write[+S[a] <: Schema.Write[?, a], -A]:
    def self: Annotation[Schema.Write.Of[S, A]]

  object Write:
    type Of[+S[a] <: Schema.Write[?, a], -A] = CoerceBase.Write[S, A] | CollectionBase.Write[S, A] |
      ConstantBase.Write[S, A] | DictionaryBase.Write[S, A] | EnumerationBase.Write[S, A] | NullishBase.Write[S, A] |
      PrimitiveBase.Write[A] | RecordBase.Write[Schema.Field.Write[S, *], A] | TupleBase.Write[S, A] |
      UnionBase.Write[Schema.Branch.Write[S, *], A]

    def apply[S[a] <: Schema.Write[?, a], A](annotation: Annotation[Schema.Write.Of[S, A]]): Schema.Write[S, A] =
      annotation.self match
        case self: CoerceBase.Write[S, A]      => Schema.Coerce.Write(annotation.copy(self = self))
        case self: CollectionBase.Write[S, A]  => Schema.Collection.Write(annotation.copy(self = self))
        case self: ConstantBase.Write[S, A]    => Schema.Constant.Write(annotation.copy(self = self))
        case self: DictionaryBase.Write[S, A]  => Schema.Dictionary.Write(annotation.copy(self = self))
        case self: EnumerationBase.Write[S, A] => Schema.Enumeration.Write(annotation.copy(self = self))
        case self: NullishBase.Write[S, A]     => Schema.Nullish.Write(annotation.copy(self = self))
        case self: PrimitiveBase.Write[A]      => Schema.Primitive.Write(annotation.copy(self = self))
        case self: RecordBase.Write[Schema.Field.Write[S, *], A] => Schema.Record.Write(annotation.copy(self = self))
        case self: TupleBase.Write[S, A]                         => Schema.Tuple.Write(annotation.copy(self = self))
        case self: UnionBase.Write[Schema.Branch.Write[S, *], A] => Schema.Union.Write(annotation.copy(self = self))

    def unapply[S[a] <: Schema.Write[?, a], A](schema: Schema.Write[S, A]): Annotation[Schema.Write.Of[S, A]] =
      schema.self

    given [S[a] <: Schema.Write[?, a]]: Contravariant[Schema.Write[S, *]] with
      override def contramap[A, B](fa: Schema.Write[S, A])(f: B => A): Schema.Write[S, B] = fa match
        case schema: Schema.Coerce.Write[S, A]      => schema.contramap(f)
        case schema: Schema.Collection.Write[S, A]  => schema.contramap(f)
        case schema: Schema.Constant.Write[S, A]    => schema.contramap(f)
        case schema: Schema.Dictionary.Write[S, A]  => schema.contramap(f)
        case schema: Schema.Enumeration.Write[S, A] => schema.contramap(f)
        case schema: Schema.Nullish.Write[S, A]     => schema.contramap(f)
        case schema: Schema.Primitive.Write[A]      => schema.contramap(f)
        case schema: Schema.Record.Write[S, A]      => schema.contramap(f)
        case schema: Schema.Tuple.Write[S, A]       => schema.contramap(f)
        case schema: Schema.Union.Write[S, A]       => schema.contramap(f)

    given [S[a] <: Schema.Write[?, a], A]: Annotated[Schema.Write[S, A]] =
      Annotated[Annotation[Schema.Write.Of[S, A]]].imap(Schema.Write.apply)(_.self)

  sealed abstract class Coerce[+S[a] <: Schema[?, a], A]
      extends Schema[S, A],
        Schema.Coerce.Read[S, A],
        Schema.Coerce.Write[S, A]:
    override def self: Annotation[CoerceBase[S, A]]

  object Coerce:
    sealed trait Read[+S[a] <: Schema.Read[?, a], +A] extends Schema.Read[S, A]:
      override def self: Annotation[CoerceBase.Read[S, A]]

    object Read:
      def apply[S[a] <: Schema.Read[?, a], A](
          annotation: Annotation[CoerceBase.Read[S, A]]
      ): Schema.Coerce.Read[S, A] = new Read[S, A]:
        override def self: Annotation[CoerceBase.Read[S, A]] = annotation

      def unapply[S[a] <: Schema.Read[?, a], A](
          schema: Schema.Coerce.Read[S, A]
      ): Annotation[CoerceBase.Read[S, A]] = schema.self

      given [S[a] <: Schema.Read[?, a]]: Functor[Schema.Coerce.Read[S, *]] =
        Functor[[a] =>> Annotation[CoerceBase.Read[S, a]]].imapK([A] =>
          (annotation: Annotation[CoerceBase.Read[S, A]]) => Schema.Coerce.Read(annotation)
        )([A] => (schema: Schema.Coerce.Read[S, A]) => schema.self)

      given [S[a] <: Schema.Read[?, a], A]: Annotated[Schema.Coerce.Read[S, A]] =
        Annotated[Annotation[CoerceBase.Read[S, A]]].imap(Schema.Coerce.Read.apply)(_.self)

      given [S[a] <: Schema.Read[?, a]]: Self.Coerce.Read[Schema.Coerce.Read, S] = Self.Coerce
        .Read[[s[a] <: S[a], a] =>> Annotation[CoerceBase.Read[s, a]], S]
        .imapK([s[a] <: S[a], a] => (annotation: Annotation[CoerceBase.Read[s, a]]) => Read(annotation))(
          [s[a] <: S[a], a] => (schema: Schema.Coerce.Read[s, a]) => schema.self
        )

    sealed trait Write[+S[a] <: Schema.Write[?, a], -A] extends Schema.Write[S, A]:
      override def self: Annotation[CoerceBase.Write[S, A]]

    object Write:
      def apply[S[a] <: Schema.Write[?, a], A](
          annotation: Annotation[CoerceBase.Write[S, A]]
      ): Schema.Coerce.Write[S, A] = new Write[S, A]:
        override def self: Annotation[CoerceBase.Write[S, A]] = annotation

      def unapply[S[a] <: Schema.Write[?, a], A](
          schema: Schema.Coerce.Write[S, A]
      ): Annotation[CoerceBase.Write[S, A]] = schema.self

      given [S[a] <: Schema.Write[?, a]]: Contravariant[Schema.Coerce.Write[S, *]] =
        Contravariant[[a] =>> Annotation[CoerceBase.Write[S, a]]].imapK([A] =>
          (annotation: Annotation[CoerceBase.Write[S, A]]) => Schema.Coerce.Write(annotation)
        )([A] => (schema: Schema.Coerce.Write[S, A]) => schema.self)

      given [S[a] <: Schema.Write[?, a], A]: Annotated[Schema.Coerce.Write[S, A]] =
        Annotated[Annotation[CoerceBase.Write[S, A]]].imap(Schema.Coerce.Write.apply)(_.self)

      given [S[a] <: Schema.Write[?, a]]: Self.Coerce.Write[Schema.Coerce.Write, S] = Self.Coerce
        .Write[[s[a] <: S[a], a] =>> Annotation[CoerceBase.Write[s, a]], S]
        .imapK([s[a] <: S[a], a] => (annotation: Annotation[CoerceBase.Write[s, a]]) => Write(annotation))(
          [s[a] <: S[a], a] => (schema: Schema.Coerce.Write[s, a]) => schema.self
        )

    def apply[S[a] <: Schema[?, a], A](annotation: Annotation[CoerceBase[S, A]]): Schema.Coerce[S, A] =
      new Coerce[S, A]:
        override def self: Annotation[CoerceBase[S, A]] = annotation

    def unapply[S[a] <: Schema[?, a], A](schema: Schema.Coerce[S, A]): Annotation[CoerceBase[S, A]] = schema.self

    given [S[a] <: Schema[?, a]]: Invariant[Schema.Coerce[S, *]] =
      Invariant[[a] =>> Annotation[CoerceBase[S, a]]].imapK([A] =>
        (annotation: Annotation[CoerceBase[S, A]]) => Coerce(annotation)
      )([A] => (schema: Coerce[S, A]) => schema.self)

    given [S[a] <: Schema[?, a], A]: Annotated[Schema.Coerce[S, A]] =
      Annotated[Annotation[CoerceBase[S, A]]].imap(Coerce.apply)(_.self)

    given [S[a] <: Schema[?, a]]: Self.Coerce[Schema.Coerce, S] = Self
      .Coerce[[s[a] <: S[a], a] =>> Annotation[CoerceBase[s, a]], S]
      .imapK([s[a] <: Schema[?, a], a] => (annotation: Annotation[CoerceBase[s, a]]) => Coerce(annotation))(
        [s[a] <: Schema[?, a], a] => (schema: Coerce[s, a]) => schema.self
      )

  sealed abstract class Collection[+S[a] <: Schema[?, a], A]
      extends Schema[S, A],
        Schema.Collection.Read[S, A],
        Schema.Collection.Write[S, A]:
    override def self: Annotation[CollectionBase[S, A]]

  object Collection:
    sealed trait Read[+S[a] <: Schema.Read[?, a], +A] extends Schema.Read[S, A]:
      override def self: Annotation[CollectionBase.Read[S, A]]

    object Read:
      def apply[S[a] <: Schema.Read[?, a], A](
          annotation: Annotation[CollectionBase.Read[S, A]]
      ): Schema.Collection.Read[S, A] = new Read[S, A]:
        override def self: Annotation[CollectionBase.Read[S, A]] = annotation

      def unapply[S[a] <: Schema.Read[?, a], A](
          schema: Schema.Collection.Read[S, A]
      ): Annotation[CollectionBase.Read[S, A]] = schema.self

      given [S[a] <: Schema.Read[?, a], A]: Annotated[Schema.Collection.Read[S, A]] =
        Annotated[Annotation[CollectionBase.Read[S, A]]].imap(Schema.Collection.Read.apply)(_.self)

      given [S[a] <: Schema.Read[?, a]]: Functor[Schema.Collection.Read[S, *]] =
        Functor[[a] =>> Annotation[CollectionBase.Read[S, a]]].imapK([A] =>
          (annotation: Annotation[CollectionBase.Read[S, A]]) => Schema.Collection.Read(annotation)
        )([A] => (schema: Schema.Collection.Read[S, A]) => schema.self)

      given [S[a] <: Schema.Read[?, a]]: Self.Collection.Read[Schema.Collection.Read, S] = Self.Collection
        .Read[[s[a] <: S[a], a] =>> Annotation[CollectionBase.Read[s, a]], S]
        .imapK([s[a] <: S[a], a] => (annotation: Annotation[CollectionBase.Read[s, a]]) => Read(annotation))(
          [s[a] <: S[a], a] => (schema: Schema.Collection.Read[s, a]) => schema.self
        )

    sealed trait Write[+S[a] <: Schema.Write[?, a], -A] extends Schema.Write[S, A]:
      override def self: Annotation[CollectionBase.Write[S, A]]

    object Write:
      def apply[S[a] <: Schema.Write[?, a], A](
          annotation: Annotation[CollectionBase.Write[S, A]]
      ): Schema.Collection.Write[S, A] = new Write[S, A]:
        override def self: Annotation[CollectionBase.Write[S, A]] = annotation

      def unapply[S[a] <: Schema.Write[?, a], A](
          schema: Schema.Collection.Write[S, A]
      ): Annotation[CollectionBase.Write[S, A]] = schema.self

      given [S[a] <: Schema.Write[?, a], A]: Annotated[Schema.Collection.Write[S, A]] =
        Annotated[Annotation[CollectionBase.Write[S, A]]].imap(Schema.Collection.Write.apply)(_.self)

      given [S[a] <: Schema.Write[?, a]]: Contravariant[Schema.Collection.Write[S, *]] =
        Contravariant[[a] =>> Annotation[CollectionBase.Write[S, a]]].imapK([A] =>
          (annotation: Annotation[CollectionBase.Write[S, A]]) => Schema.Collection.Write(annotation)
        )([A] => (schema: Schema.Collection.Write[S, A]) => schema.self)

      given [S[a] <: Schema.Write[?, a]]: Self.Collection.Write[Schema.Collection.Write, S] = Self.Collection
        .Write[[s[a] <: S[a], a] =>> Annotation[CollectionBase.Write[s, a]], S]
        .imapK([s[a] <: S[a], a] => (annotation: Annotation[CollectionBase.Write[s, a]]) => Write(annotation))(
          [s[a] <: S[a], a] => (schema: Schema.Collection.Write[s, a]) => schema.self
        )

    def apply[S[a] <: Schema[?, a], A](annotation: Annotation[CollectionBase[S, A]]): Schema.Collection[S, A] =
      new Collection[S, A]:
        override def self: Annotation[CollectionBase[S, A]] = annotation

    def unapply[S[a] <: Schema[?, a], A](schema: Schema.Collection[S, A]): Annotation[CollectionBase[S, A]] =
      schema.self

    given [S[a] <: Schema[?, a], A]: Annotated[Schema.Collection[S, A]] =
      Annotated[Annotation[CollectionBase[S, A]]].imap(Schema.Collection.apply)(_.self)

    given [S[a] <: Schema[?, a]]: Invariant[Schema.Collection[S, *]] =
      Invariant[[a] =>> Annotation[CollectionBase[S, a]]].imapK([A] =>
        (annotation: Annotation[CollectionBase[S, A]]) => Schema.Collection(annotation)
      )([A] => (schema: Schema.Collection[S, A]) => schema.self)

    given [S[a] <: Schema[?, a]]: Self.Collection[Schema.Collection, S] = Self
      .Collection[[s[a] <: S[a], a] =>> Annotation[CollectionBase[s, a]], S]
      .imapK([s[a] <: S[a], a] => (annotation: Annotation[CollectionBase[s, a]]) => Collection(annotation))(
        [s[a] <: S[a], a] => (schema: Collection[s, a]) => schema.self
      )

  sealed abstract class Constant[+S[a] <: Schema[?, a], A]
      extends Schema[S, A],
        Schema.Constant.Read[S, A],
        Schema.Constant.Write[S, A]:
    override def self: Annotation[ConstantBase[S, A]]

  object Constant:
    sealed trait Read[+S[a] <: Schema.Read[?, a], +A] extends Schema.Read[S, A]:
      override def self: Annotation[ConstantBase.Read[S, A]]

    object Read:
      def apply[S[a] <: Schema.Read[?, a], A](
          annotation: Annotation[ConstantBase.Read[S, A]]
      ): Schema.Constant.Read[S, A] = new Read[S, A]:
        override def self: Annotation[ConstantBase.Read[S, A]] = annotation

      def unapply[S[a] <: Schema.Read[?, a], A](
          schema: Schema.Constant.Read[S, A]
      ): Annotation[ConstantBase.Read[S, A]] = schema.self

      given [S[a] <: Schema.Read[?, a], A]: Annotated[Schema.Constant.Read[S, A]] =
        Annotated[Annotation[ConstantBase.Read[S, A]]].imap(Schema.Constant.Read.apply)(_.self)

      given [S[a] <: Schema.Read[?, a]]: Functor[Schema.Constant.Read[S, *]] =
        Functor[[a] =>> Annotation[ConstantBase.Read[S, a]]].imapK([A] =>
          (annotation: Annotation[ConstantBase.Read[S, A]]) => Schema.Constant.Read(annotation)
        )([A] => (schema: Schema.Constant.Read[S, A]) => schema.self)

      given [S[a] <: Schema.Read[?, a]]: Self.Constant.Read[Schema.Constant.Read, S] = Self.Constant
        .Read[[s[a] <: S[a], a] =>> Annotation[ConstantBase.Read[s, a]], S]
        .imapK([s[a] <: S[a], a] => (annotation: Annotation[ConstantBase.Read[s, a]]) => Read(annotation))(
          [s[a] <: S[a], a] => (schema: Schema.Constant.Read[s, a]) => schema.self
        )

    sealed trait Write[+S[a] <: Schema.Write[?, a], -A] extends Schema.Write[S, A]:
      override def self: Annotation[ConstantBase.Write[S, A]]

    object Write:
      def apply[S[a] <: Schema.Write[?, a], A](
          annotation: Annotation[ConstantBase.Write[S, A]]
      ): Schema.Constant.Write[S, A] = new Write[S, A]:
        override def self: Annotation[ConstantBase.Write[S, A]] = annotation

      def unapply[S[a] <: Schema.Write[?, a], A](
          schema: Schema.Constant.Write[S, A]
      ): Annotation[ConstantBase.Write[S, A]] = schema.self

      given [S[a] <: Schema.Write[?, a], A]: Annotated[Schema.Constant.Write[S, A]] =
        Annotated[Annotation[ConstantBase.Write[S, A]]].imap(Schema.Constant.Write.apply)(_.self)

      given [S[a] <: Schema.Write[?, a]]: Contravariant[Schema.Constant.Write[S, *]] =
        Contravariant[[a] =>> Annotation[ConstantBase.Write[S, a]]].imapK([A] =>
          (annotation: Annotation[ConstantBase.Write[S, A]]) => Schema.Constant.Write(annotation)
        )([A] => (schema: Schema.Constant.Write[S, A]) => schema.self)

      given [S[a] <: Schema.Write[?, a]]: Self.Constant.Write[Schema.Constant.Write, S] = Self.Constant
        .Write[[s[a] <: S[a], a] =>> Annotation[ConstantBase.Write[s, a]], S]
        .imapK([s[a] <: S[a], a] => (annotation: Annotation[ConstantBase.Write[s, a]]) => Write(annotation))(
          [s[a] <: S[a], a] => (schema: Schema.Constant.Write[s, a]) => schema.self
        )

    def apply[S[a] <: Schema[?, a], A](annotation: Annotation[ConstantBase[S, A]]): Schema.Constant[S, A] =
      new Constant[S, A]:
        override def self: Annotation[ConstantBase[S, A]] = annotation

    def unapply[S[a] <: Schema[?, a], A](schema: Schema.Constant[S, A]): Annotation[ConstantBase[S, A]] =
      schema.self

    given [S[a] <: Schema[?, a], A]: Annotated[Schema.Constant[S, A]] =
      Annotated[Annotation[ConstantBase[S, A]]].imap(Schema.Constant.apply)(_.self)

    given [S[a] <: Schema[?, a]]: Invariant[Schema.Constant[S, *]] =
      Invariant[[a] =>> Annotation[ConstantBase[S, a]]].imapK([A] =>
        (annotation: Annotation[ConstantBase[S, A]]) => Schema.Constant(annotation)
      )([A] => (schema: Schema.Constant[S, A]) => schema.self)

    given [S[a] <: Schema[?, a]]: Self.Constant[Schema.Constant, S] = Self
      .Constant[[s[a] <: S[a], a] =>> Annotation[ConstantBase[s, a]], S]
      .imapK([s[a] <: S[a], a] => (annotation: Annotation[ConstantBase[s, a]]) => Constant(annotation))(
        [s[a] <: S[a], a] => (schema: Constant[s, a]) => schema.self
      )

  sealed abstract class Dictionary[+S[a] <: Schema[?, a], A]
      extends Schema[S, A],
        Schema.Dictionary.Read[S, A],
        Schema.Dictionary.Write[S, A]:
    override def self: Annotation[DictionaryBase[S, A]]

  object Dictionary:
    sealed trait Read[+S[a] <: Schema.Read[?, a], +A] extends Schema.Read[S, A]:
      override def self: Annotation[DictionaryBase.Read[S, A]]

    object Read:
      def apply[S[a] <: Schema.Read[?, a], A](
          annotation: Annotation[DictionaryBase.Read[S, A]]
      ): Schema.Dictionary.Read[S, A] = new Read[S, A]:
        override def self: Annotation[DictionaryBase.Read[S, A]] = annotation

      def unapply[S[a] <: Schema.Read[?, a], A](
          schema: Schema.Dictionary.Read[S, A]
      ): Annotation[DictionaryBase.Read[S, A]] = schema.self

      given [S[a] <: Schema.Read[?, a], A]: Annotated[Schema.Dictionary.Read[S, A]] =
        Annotated[Annotation[DictionaryBase.Read[S, A]]].imap(Schema.Dictionary.Read.apply)(_.self)

      given [S[a] <: Schema.Read[?, a]]: Functor[Schema.Dictionary.Read[S, *]] =
        Functor[[a] =>> Annotation[DictionaryBase.Read[S, a]]].imapK([A] =>
          (annotation: Annotation[DictionaryBase.Read[S, A]]) => Schema.Dictionary.Read(annotation)
        )([A] => (schema: Schema.Dictionary.Read[S, A]) => schema.self)

      given [S[a] <: Schema.Read[?, a]]: Self.Dictionary.Read[Schema.Dictionary.Read, S] = Self.Dictionary
        .Read[[s[a] <: S[a], a] =>> Annotation[DictionaryBase.Read[s, a]], S]
        .imapK([s[a] <: S[a], a] => (annotation: Annotation[DictionaryBase.Read[s, a]]) => Read(annotation))(
          [s[a] <: S[a], a] => (schema: Schema.Dictionary.Read[s, a]) => schema.self
        )

    sealed trait Write[+S[a] <: Schema.Write[?, a], -A] extends Schema.Write[S, A]:
      override def self: Annotation[DictionaryBase.Write[S, A]]

    object Write:
      def apply[S[a] <: Schema.Write[?, a], A](
          annotation: Annotation[DictionaryBase.Write[S, A]]
      ): Schema.Dictionary.Write[S, A] = new Write[S, A]:
        override def self: Annotation[DictionaryBase.Write[S, A]] = annotation

      def unapply[S[a] <: Schema.Write[?, a], A](
          schema: Schema.Dictionary.Write[S, A]
      ): Annotation[DictionaryBase.Write[S, A]] = schema.self

      given [S[a] <: Schema.Write[?, a], A]: Annotated[Schema.Dictionary.Write[S, A]] =
        Annotated[Annotation[DictionaryBase.Write[S, A]]].imap(Schema.Dictionary.Write.apply)(_.self)

      given [S[a] <: Schema.Write[?, a]]: Contravariant[Schema.Dictionary.Write[S, *]] =
        Contravariant[[a] =>> Annotation[DictionaryBase.Write[S, a]]].imapK([A] =>
          (annotation: Annotation[DictionaryBase.Write[S, A]]) => Schema.Dictionary.Write(annotation)
        )([A] => (schema: Schema.Dictionary.Write[S, A]) => schema.self)

      given [S[a] <: Schema.Write[?, a]]: Self.Dictionary.Write[Schema.Dictionary.Write, S] = Self.Dictionary
        .Write[[s[a] <: S[a], a] =>> Annotation[DictionaryBase.Write[s, a]], S]
        .imapK([s[a] <: S[a], a] => (annotation: Annotation[DictionaryBase.Write[s, a]]) => Write(annotation))(
          [s[a] <: S[a], a] => (schema: Schema.Dictionary.Write[s, a]) => schema.self
        )

    def apply[S[a] <: Schema[?, a], A](annotation: Annotation[DictionaryBase[S, A]]): Schema.Dictionary[S, A] =
      new Dictionary[S, A]:
        override def self: Annotation[DictionaryBase[S, A]] = annotation

    def unapply[S[a] <: Schema[?, a], A](schema: Schema.Dictionary[S, A]): Annotation[DictionaryBase[S, A]] =
      schema.self

    given [S[a] <: Schema[?, a], A]: Annotated[Schema.Dictionary[S, A]] =
      Annotated[Annotation[DictionaryBase[S, A]]].imap(Schema.Dictionary.apply)(_.self)

    given [S[a] <: Schema[?, a]]: Invariant[Schema.Dictionary[S, *]] =
      Invariant[[a] =>> Annotation[DictionaryBase[S, a]]].imapK([A] =>
        (annotation: Annotation[DictionaryBase[S, A]]) => Schema.Dictionary(annotation)
      )([A] => (schema: Schema.Dictionary[S, A]) => schema.self)

    given [S[a] <: Schema[?, a]]: Self.Dictionary[Schema.Dictionary, S] = Self
      .Dictionary[[s[a] <: S[a], a] =>> Annotation[DictionaryBase[s, a]], S]
      .imapK([s[a] <: S[a], a] => (annotation: Annotation[DictionaryBase[s, a]]) => Dictionary(annotation))(
        [s[a] <: S[a], a] => (schema: Dictionary[s, a]) => schema.self
      )

  sealed abstract class Enumeration[+S[a] <: Schema[?, a], A]
      extends Schema[S, A],
        Schema.Enumeration.Read[S, A],
        Schema.Enumeration.Write[S, A]:
    override def self: Annotation[EnumerationBase[S, A]]

  object Enumeration:
    sealed trait Read[+S[a] <: Schema.Read[?, a], +A] extends Schema.Read[S, A]:
      override def self: Annotation[EnumerationBase.Read[S, A]]

    object Read:
      def apply[S[a] <: Schema.Read[?, a], A](
          annotation: Annotation[EnumerationBase.Read[S, A]]
      ): Schema.Enumeration.Read[S, A] = new Read[S, A]:
        override def self: Annotation[EnumerationBase.Read[S, A]] = annotation

      def unapply[S[a] <: Schema.Read[?, a], A](
          schema: Schema.Enumeration.Read[S, A]
      ): Annotation[EnumerationBase.Read[S, A]] = schema.self

      given [S[a] <: Schema.Read[?, a], A]: Annotated[Schema.Enumeration.Read[S, A]] =
        Annotated[Annotation[EnumerationBase.Read[S, A]]].imap(Schema.Enumeration.Read.apply)(_.self)

      given [S[a] <: Schema.Read[?, a]]: Functor[Schema.Enumeration.Read[S, *]] =
        Functor[[a] =>> Annotation[EnumerationBase.Read[S, a]]].imapK([A] =>
          (annotation: Annotation[EnumerationBase.Read[S, A]]) => Schema.Enumeration.Read(annotation)
        )([A] => (schema: Schema.Enumeration.Read[S, A]) => schema.self)

      given [S[a] <: Schema.Read[?, a]]: Self.Enumeration.Read[Schema.Enumeration.Read, S] = Self.Enumeration
        .Read[[s[a] <: S[a], a] =>> Annotation[EnumerationBase.Read[s, a]], S]
        .imapK([s[a] <: S[a], a] => (annotation: Annotation[EnumerationBase.Read[s, a]]) => Read(annotation))(
          [s[a] <: S[a], a] => (schema: Schema.Enumeration.Read[s, a]) => schema.self
        )

    sealed trait Write[+S[a] <: Schema.Write[?, a], -A] extends Schema.Write[S, A]:
      override def self: Annotation[EnumerationBase.Write[S, A]]

    object Write:
      def apply[S[a] <: Schema.Write[?, a], A](
          annotation: Annotation[EnumerationBase.Write[S, A]]
      ): Schema.Enumeration.Write[S, A] = new Write[S, A]:
        override def self: Annotation[EnumerationBase.Write[S, A]] = annotation

      def unapply[S[a] <: Schema.Write[?, a], A](
          schema: Schema.Enumeration.Write[S, A]
      ): Annotation[EnumerationBase.Write[S, A]] = schema.self

      given [S[a] <: Schema.Write[?, a], A]: Annotated[Schema.Enumeration.Write[S, A]] =
        Annotated[Annotation[EnumerationBase.Write[S, A]]].imap(Schema.Enumeration.Write.apply)(_.self)

      given [S[a] <: Schema.Write[?, a]]: Contravariant[Schema.Enumeration.Write[S, *]] =
        Contravariant[[a] =>> Annotation[EnumerationBase.Write[S, a]]].imapK([A] =>
          (annotation: Annotation[EnumerationBase.Write[S, A]]) => Schema.Enumeration.Write(annotation)
        )([A] => (schema: Schema.Enumeration.Write[S, A]) => schema.self)

      given [S[a] <: Schema.Write[?, a]]: Self.Enumeration.Write[Schema.Enumeration.Write, S] = Self.Enumeration
        .Write[[s[a] <: S[a], a] =>> Annotation[EnumerationBase.Write[s, a]], S]
        .imapK([s[a] <: S[a], a] => (annotation: Annotation[EnumerationBase.Write[s, a]]) => Write(annotation))(
          [s[a] <: S[a], a] => (schema: Schema.Enumeration.Write[s, a]) => schema.self
        )

    def apply[S[a] <: Schema[?, a], A](annotation: Annotation[EnumerationBase[S, A]]): Schema.Enumeration[S, A] =
      new Enumeration[S, A]:
        override def self: Annotation[EnumerationBase[S, A]] = annotation

    def unapply[S[a] <: Schema[?, a], A](schema: Schema.Enumeration[S, A]): Annotation[EnumerationBase[S, A]] =
      schema.self

    given [S[a] <: Schema[?, a], A]: Annotated[Schema.Enumeration[S, A]] =
      Annotated[Annotation[EnumerationBase[S, A]]].imap(Schema.Enumeration.apply)(_.self)

    given [S[a] <: Schema[?, a]]: Invariant[Schema.Enumeration[S, *]] =
      Invariant[[a] =>> Annotation[EnumerationBase[S, a]]].imapK([A] =>
        (annotation: Annotation[EnumerationBase[S, A]]) => Schema.Enumeration(annotation)
      )([A] => (schema: Schema.Enumeration[S, A]) => schema.self)

    given [S[a] <: Schema[?, a]]: Self.Enumeration[Schema.Enumeration, S] = Self
      .Enumeration[[s[a] <: S[a], a] =>> Annotation[EnumerationBase[s, a]], S]
      .imapK([s[a] <: S[a], a] => (annotation: Annotation[EnumerationBase[s, a]]) => Enumeration(annotation))(
        [s[a] <: S[a], a] => (schema: Enumeration[s, a]) => schema.self
      )

  sealed abstract class Nullish[+S[a] <: Schema[?, a], A]
      extends Schema[S, A],
        Schema.Nullish.Read[S, A],
        Schema.Nullish.Write[S, A]:
    override def self: Annotation[NullishBase[S, A]]

  object Nullish:
    sealed trait Read[+S[a] <: Schema.Read[?, a], +A] extends Schema.Read[S, A]:
      override def self: Annotation[NullishBase.Read[S, A]]

    object Read:
      def apply[S[a] <: Schema.Read[?, a], A](
          annotation: Annotation[NullishBase.Read[S, A]]
      ): Schema.Nullish.Read[S, A] = new Schema.Nullish.Read[S, A]:
        override def self: Annotation[NullishBase.Read[S, A]] = annotation

      def unapply[S[a] <: Schema.Read[?, a], A](
          schema: Schema.Nullish.Read[S, A]
      ): Annotation[NullishBase.Read[S, A]] = schema.self

      given [S[a] <: Schema.Read[?, a], A]: Annotated[Schema.Nullish.Read[S, A]] =
        Annotated[Annotation[NullishBase.Read[S, A]]].imap(Schema.Nullish.Read.apply)(_.self)

      given [S[a] <: Schema.Read[?, a]]: Functor[Schema.Nullish.Read[S, *]] =
        Functor[[a] =>> Annotation[NullishBase.Read[S, a]]].imapK([A] =>
          (annotation: Annotation[NullishBase.Read[S, A]]) => Schema.Nullish.Read(annotation)
        )([A] => (schema: Schema.Nullish.Read[S, A]) => schema.self)

      given [S[a] <: Schema.Read[?, a]]: Self.Nullish.Read[Schema.Nullish.Read, S] = Self.Nullish
        .Read[[s[a] <: S[a], a] =>> Annotation[NullishBase.Read[s, a]], S]
        .imapK([s[a] <: S[a], a] => (annotation: Annotation[NullishBase.Read[s, a]]) => Read(annotation))(
          [s[a] <: S[a], a] => (schema: Schema.Nullish.Read[s, a]) => schema.self
        )

    sealed trait Write[+S[a] <: Schema.Write[?, a], -A] extends Schema.Write[S, A]:
      override def self: Annotation[NullishBase.Write[S, A]]

    object Write:
      def apply[S[a] <: Schema.Write[?, a], A](
          annotation: Annotation[NullishBase.Write[S, A]]
      ): Schema.Nullish.Write[S, A] = new Schema.Nullish.Write[S, A]:
        override def self: Annotation[NullishBase.Write[S, A]] = annotation

      def unapply[S[a] <: Schema.Write[?, a], A](
          schema: Schema.Nullish.Write[S, A]
      ): Annotation[NullishBase.Write[S, A]] = schema.self

      given [S[a] <: Schema.Write[?, a], A]: Annotated[Schema.Nullish.Write[S, A]] =
        Annotated[Annotation[NullishBase.Write[S, A]]].imap(Schema.Nullish.Write.apply)(_.self)

      given [S[a] <: Schema.Write[?, a]]: Contravariant[Schema.Nullish.Write[S, *]] =
        Contravariant[[a] =>> Annotation[NullishBase.Write[S, a]]].imapK([A] =>
          (annotation: Annotation[NullishBase.Write[S, A]]) => Schema.Nullish.Write(annotation)
        )([A] => (schema: Schema.Nullish.Write[S, A]) => schema.self)

      given [S[a] <: Schema.Write[?, a]]: Self.Nullish.Write[Schema.Nullish.Write, S] = Self.Nullish
        .Write[[s[a] <: S[a], a] =>> Annotation[NullishBase.Write[s, a]], S]
        .imapK([s[a] <: S[a], a] => (annotation: Annotation[NullishBase.Write[s, a]]) => Write(annotation))(
          [s[a] <: S[a], a] => (schema: Schema.Nullish.Write[s, a]) => schema.self
        )

    def apply[S[a] <: Schema[?, a], A](annotation: Annotation[NullishBase[S, A]]): Schema.Nullish[S, A] =
      new Nullish[S, A]:
        override def self: Annotation[NullishBase[S, A]] = annotation

    def unapply[S[a] <: Schema[?, a], A](schema: Schema.Nullish[S, A]): Annotation[NullishBase[S, A]] =
      schema.self

    given [S[a] <: Schema[?, a], A]: Annotated[Schema.Nullish[S, A]] =
      Annotated[Annotation[NullishBase[S, A]]].imap(Schema.Nullish.apply)(_.self)

    given [S[a] <: Schema[?, a]]: Invariant[Schema.Nullish[S, *]] =
      Invariant[[a] =>> Annotation[NullishBase[S, a]]].imapK([A] =>
        (annotation: Annotation[NullishBase[S, A]]) => Schema.Nullish(annotation)
      )([A] => (schema: Schema.Nullish[S, A]) => schema.self)

    given [S[a] <: Schema[?, a]]: Self.Nullish[Schema.Nullish, S] = Self
      .Nullish[[s[a] <: S[a], a] =>> Annotation[NullishBase[s, a]], S]
      .imapK([s[a] <: S[a], a] => (annotation: Annotation[NullishBase[s, a]]) => Nullish(annotation))(
        [s[a] <: S[a], a] => (schema: Nullish[s, a]) => schema.self
      )

  sealed abstract class Record[+S[a] <: Schema[?, a], A]
      extends Schema[S, A],
        Schema.Record.Read[S, A],
        Schema.Record.Write[S, A]:
    override def self: Annotation[RecordBase[Schema.Field[S, *], A]]

  object Record:
    sealed trait Read[+S[a] <: Schema.Read[?, a], +A] extends Schema.Read[S, A]:
      override def self: Annotation[RecordBase.Read[Schema.Field.Read[S, *], A]]

    object Read:
      def apply[S[a] <: Schema.Read[?, a], A](
          annotation: Annotation[RecordBase.Read[Schema.Field.Read[S, *], A]]
      ): Schema.Record.Read[S, A] = new Read[S, A]:
        override def self: Annotation[RecordBase.Read[Schema.Field.Read[S, *], A]] = annotation

      def unapply[S[a] <: Schema.Read[?, a], A](
          schema: Schema.Record.Read[S, A]
      ): Annotation[RecordBase.Read[Schema.Field.Read[S, *], A]] = schema.self

      given [S[a] <: Schema.Read[?, a], A]: Annotated[Schema.Record.Read[S, A]] =
        Annotated[Annotation[RecordBase.Read[Schema.Field.Read[S, *], A]]].imap(Schema.Record.Read.apply)(_.self)

      given [S[a] <: Schema.Read[?, a]]: Functor[Schema.Record.Read[S, *]] =
        Functor[[a] =>> Annotation[RecordBase.Read[Schema.Field.Read[S, *], a]]].imapK([A] =>
          (annotation: Annotation[RecordBase.Read[Schema.Field.Read[S, *], A]]) => Schema.Record.Read(annotation)
        )([A] => (schema: Schema.Record.Read[S, A]) => schema.self)

      given Self.Record.Read[Schema.Record.Read, Schema.Field.Read, Schema.Read[?, *]] = Self.Record
        .Read[
          [s[a] <: Schema.Read[?, a], a] =>> Annotation[RecordBase.Read[Schema.Field.Read[s, *], a]],
          Schema.Field.Read,
          Schema.Read[?, *]
        ]
        .imapK([s[a] <: Schema.Read[?, a], a] =>
          (annotation: Annotation[RecordBase.Read[Schema.Field.Read[s, *], a]]) => Read(annotation)
        )([s[a] <: Schema.Read[?, a], a] => (schema: Read[s, a]) => schema.self)

    sealed trait Write[+S[a] <: Schema.Write[?, a], -A] extends Schema.Write[S, A]:
      override def self: Annotation[RecordBase.Write[Schema.Field.Write[S, *], A]]

    object Write:
      def apply[S[a] <: Schema.Write[?, a], A](
          annotation: Annotation[RecordBase.Write[Schema.Field.Write[S, *], A]]
      ): Schema.Record.Write[S, A] = new Write[S, A]:
        override def self: Annotation[RecordBase.Write[Schema.Field.Write[S, *], A]] = annotation

      def unapply[S[a] <: Schema.Write[?, a], A](
          schema: Schema.Record.Write[S, A]
      ): Annotation[RecordBase.Write[Schema.Field.Write[S, *], A]] = schema.self

      given [S[a] <: Schema.Write[?, a], A]: Annotated[Schema.Record.Write[S, A]] =
        Annotated[Annotation[RecordBase.Write[Schema.Field.Write[S, *], A]]].imap(Schema.Record.Write.apply)(_.self)

      given [S[a] <: Schema.Write[?, a]]: Contravariant[Schema.Record.Write[S, *]] =
        Contravariant[[a] =>> Annotation[RecordBase.Write[Schema.Field.Write[S, *], a]]].imapK([A] =>
          (annotation: Annotation[RecordBase.Write[Schema.Field.Write[S, *], A]]) => Schema.Record.Write(annotation)
        )([A] => (schema: Schema.Record.Write[S, A]) => schema.self)

      given Self.Record.Write[Schema.Record.Write, Schema.Field.Write, Schema.Write[?, *]] = Self.Record
        .Write[
          [s[a] <: Schema.Write[?, a], a] =>> Annotation[RecordBase.Write[Schema.Field.Write[s, *], a]],
          Schema.Field.Write,
          Schema.Write[?, *]
        ]
        .imapK([s[a] <: Schema.Write[?, a], a] =>
          (annotation: Annotation[RecordBase.Write[Schema.Field.Write[s, *], a]]) => Write(annotation)
        )([s[a] <: Schema.Write[?, a], a] => (schema: Write[s, a]) => schema.self)

    def apply[S[a] <: Schema[?, a], A](
        annotation: Annotation[RecordBase[Schema.Field[S, *], A]]
    ): Schema.Record[S, A] = new Record[S, A]:
      override def self: Annotation[RecordBase[Schema.Field[S, *], A]] = annotation

    def unapply[S[a] <: Schema[?, a], A](schema: Schema.Record[S, A]): Annotation[RecordBase[Schema.Field[S, *], A]] =
      schema.self

    given [S[a] <: Schema[?, a], A]: Annotated[Schema.Record[S, A]] =
      Annotated[Annotation[RecordBase[Schema.Field[S, *], A]]].imap(Schema.Record.apply)(_.self)

    given [S[a] <: Schema[?, a]]: Invariant[Schema.Record[S, *]] =
      Invariant[[a] =>> Annotation[RecordBase[Schema.Field[S, *], a]]].imapK([A] =>
        (annotation: Annotation[RecordBase[Schema.Field[S, *], A]]) => Record(annotation)
      )([A] => (schema: Schema.Record[S, A]) => schema.self)

    given Self.Record[Schema.Record, Schema.Field, Schema[?, *]] = Self
      .Record[
        [s[a] <: Schema[?, a], a] =>> Annotation[RecordBase[Schema.Field[s, *], a]],
        Schema.Field,
        Schema[?, *]
      ]
      .imapK([s[a] <: Schema[?, a], a] =>
        (annotation: Annotation[RecordBase[Schema.Field[s, *], a]]) => Record(annotation)
      )([s[a] <: Schema[?, a], a] => (schema: Record[s, a]) => schema.self)

  sealed abstract class Primitive[A] extends Schema[Nothing, A], Schema.Primitive.Read[A], Schema.Primitive.Write[A]:
    override def self: Annotation[PrimitiveBase[A]]

  object Primitive:
    sealed trait Read[+A] extends Schema.Read[Nothing, A]:
      override def self: Annotation[PrimitiveBase.Read[A]]

    object Read:
      def apply[A](annotation: Annotation[PrimitiveBase.Read[A]]): Schema.Primitive.Read[A] = annotation.self match
        case self: PrimitiveBase.Boolean.Read[A] => Boolean.Read(annotation.copy(self = self))
        case self: PrimitiveBase.Number.Read[A]  => Number.Read(annotation.copy(self = self))
        case self: PrimitiveBase.Text.Read[A]    => Text.Read(annotation.copy(self = self))

      def unapply[A](schema: Schema.Primitive.Read[A]): Annotation[PrimitiveBase.Read[A]] = schema.self

      given Functor[Schema.Primitive.Read] =
        Functor[[a] =>> Annotation[PrimitiveBase.Read[a]]].imapK([A] =>
          (annotation: Annotation[PrimitiveBase.Read[A]]) => Schema.Primitive.Read(annotation)
        )([A] => (schema: Schema.Primitive.Read[A]) => schema.self)

      given [A]: Annotated[Schema.Primitive.Read[A]] =
        Annotated[Annotation[PrimitiveBase.Read[A]]].imap(Schema.Primitive.Read.apply)(_.self)

    sealed trait Write[-A] extends Schema.Write[Nothing, A]:
      override def self: Annotation[PrimitiveBase.Write[A]]

    object Write:
      def apply[A](annotation: Annotation[PrimitiveBase.Write[A]]): Schema.Primitive.Write[A] =
        annotation.self match
          case self: PrimitiveBase.Boolean.Write[A] => Boolean.Write(annotation.copy(self = self))
          case self: PrimitiveBase.Number.Write[A]  => Number.Write(annotation.copy(self = self))
          case self: PrimitiveBase.Text.Write[A]    => Text.Write(annotation.copy(self = self))

      def unapply[A](schema: Schema.Primitive.Write[A]): Annotation[PrimitiveBase.Write[A]] = schema.self

      given Contravariant[Schema.Primitive.Write] =
        Contravariant[[a] =>> Annotation[PrimitiveBase.Write[a]]].imapK([A] =>
          (annotation: Annotation[PrimitiveBase.Write[A]]) => Schema.Primitive.Write(annotation)
        )([A] => (schema: Schema.Primitive.Write[A]) => schema.self)

      given [A]: Annotated[Schema.Primitive.Write[A]] = Annotated[Annotation[PrimitiveBase.Write[A]]]
        .imap(Schema.Primitive.Write.apply)(_.self)

    sealed abstract class Boolean[A]
        extends Schema.Primitive[A],
          Schema.Primitive.Boolean.Read[A],
          Schema.Primitive.Boolean.Write[A]:
      override def self: Annotation[PrimitiveBase.Boolean[A]]

    object Boolean:
      sealed trait Read[+A] extends Schema.Primitive.Read[A]:
        override def self: Annotation[PrimitiveBase.Boolean.Read[A]]

      object Read:
        def apply[A](annotation: Annotation[PrimitiveBase.Boolean.Read[A]]): Schema.Primitive.Boolean.Read[A] =
          new Schema.Primitive.Boolean.Read[A]:
            override def self: Annotation[PrimitiveBase.Boolean.Read[A]] = annotation

        def unapply[A](schema: Schema.Primitive.Boolean.Read[A]): Annotation[PrimitiveBase.Boolean.Read[A]] =
          schema.self

        given [A]: Annotated[Schema.Primitive.Boolean.Read[A]] =
          Annotated[Annotation[PrimitiveBase.Boolean.Read[A]]].imap(Schema.Primitive.Boolean.Read.apply)(_.self)

        given Functor[Schema.Primitive.Boolean.Read] =
          Functor[[a] =>> Annotation[PrimitiveBase.Boolean.Read[a]]].imapK([A] =>
            (annotation: Annotation[PrimitiveBase.Boolean.Read[A]]) => Schema.Primitive.Boolean.Read(annotation)
          )([A] => (schema: Schema.Primitive.Boolean.Read[A]) => schema.self)

      sealed trait Write[-A] extends Schema.Primitive.Write[A]:
        override def self: Annotation[PrimitiveBase.Boolean.Write[A]]

      object Write:
        def apply[A](annotation: Annotation[PrimitiveBase.Boolean.Write[A]]): Schema.Primitive.Boolean.Write[A] =
          new Schema.Primitive.Boolean.Write[A]:
            override def self: Annotation[PrimitiveBase.Boolean.Write[A]] = annotation

        def unapply[A](schema: Schema.Primitive.Boolean.Write[A]): Annotation[PrimitiveBase.Boolean.Write[A]] =
          schema.self

        given [A]: Annotated[Schema.Primitive.Boolean.Write[A]] =
          Annotated[Annotation[PrimitiveBase.Boolean.Write[A]]].imap(Schema.Primitive.Boolean.Write.apply)(_.self)

        given Contravariant[Schema.Primitive.Boolean.Write] =
          Contravariant[[a] =>> Annotation[PrimitiveBase.Boolean.Write[a]]].imapK([A] =>
            (annotation: Annotation[PrimitiveBase.Boolean.Write[A]]) => Schema.Primitive.Boolean.Write(annotation)
          )([A] => (schema: Schema.Primitive.Boolean.Write[A]) => schema.self)

      def apply[A](annotation: Annotation[PrimitiveBase.Boolean[A]]): Schema.Primitive.Boolean[A] = new Boolean[A]:
        override def self: Annotation[PrimitiveBase.Boolean[A]] = annotation

      def unapply[A](schema: Schema.Primitive.Boolean[A]): Annotation[PrimitiveBase.Boolean[A]] = schema.self

      given [A]: Annotated[Schema.Primitive.Boolean[A]] =
        Annotated[Annotation[PrimitiveBase.Boolean[A]]].imap(Schema.Primitive.Boolean.apply)(_.self)

      given Invariant[Schema.Primitive.Boolean] =
        Invariant[[a] =>> Annotation[PrimitiveBase.Boolean[a]]].imapK([A] =>
          (annotation: Annotation[PrimitiveBase.Boolean[A]]) => Schema.Primitive.Boolean(annotation)
        )([A] => (schema: Schema.Primitive.Boolean[A]) => schema.self)

    sealed abstract class Number[A]
        extends Schema.Primitive[A],
          Schema.Primitive.Number.Read[A],
          Schema.Primitive.Number.Write[A]:
      override def self: Annotation[PrimitiveBase.Number[A]]

    object Number:
      sealed trait Read[+A] extends Schema.Primitive.Read[A]:
        override def self: Annotation[PrimitiveBase.Number.Read[A]]

      object Read:
        def apply[A](annotation: Annotation[PrimitiveBase.Number.Read[A]]): Schema.Primitive.Number.Read[A] =
          new Schema.Primitive.Number.Read[A]:
            override def self: Annotation[PrimitiveBase.Number.Read[A]] = annotation

        def unapply[A](schema: Schema.Primitive.Number.Read[A]): Annotation[PrimitiveBase.Number.Read[A]] =
          schema.self

        given [A]: Annotated[Schema.Primitive.Number.Read[A]] =
          Annotated[Annotation[PrimitiveBase.Number.Read[A]]].imap(Schema.Primitive.Number.Read.apply)(_.self)

        given Functor[Schema.Primitive.Number.Read] =
          Functor[[a] =>> Annotation[PrimitiveBase.Number.Read[a]]].imapK([A] =>
            (annotation: Annotation[PrimitiveBase.Number.Read[A]]) => Schema.Primitive.Number.Read(annotation)
          )([A] => (schema: Schema.Primitive.Number.Read[A]) => schema.self)

      sealed trait Write[-A] extends Schema.Primitive.Write[A]:
        override def self: Annotation[PrimitiveBase.Number.Write[A]]

      object Write:
        def apply[A](annotation: Annotation[PrimitiveBase.Number.Write[A]]): Schema.Primitive.Number.Write[A] =
          new Schema.Primitive.Number.Write[A]:
            override def self: Annotation[PrimitiveBase.Number.Write[A]] = annotation

        def unapply[A](schema: Schema.Primitive.Number.Write[A]): Annotation[PrimitiveBase.Number.Write[A]] =
          schema.self

        given [A]: Annotated[Schema.Primitive.Number.Write[A]] =
          Annotated[Annotation[PrimitiveBase.Number.Write[A]]].imap(Schema.Primitive.Number.Write.apply)(_.self)

        given Contravariant[Schema.Primitive.Number.Write] =
          Contravariant[[a] =>> Annotation[PrimitiveBase.Number.Write[a]]].imapK([A] =>
            (annotation: Annotation[PrimitiveBase.Number.Write[A]]) => Schema.Primitive.Number.Write(annotation)
          )([A] => (schema: Schema.Primitive.Number.Write[A]) => schema.self)

      def apply[A](annotation: Annotation[PrimitiveBase.Number[A]]): Schema.Primitive.Number[A] = new Number[A]:
        override def self: Annotation[PrimitiveBase.Number[A]] = annotation

      def unapply[A](schema: Schema.Primitive.Number[A]): Annotation[PrimitiveBase.Number[A]] = schema.self

      given [A]: Annotated[Schema.Primitive.Number[A]] =
        Annotated[Annotation[PrimitiveBase.Number[A]]].imap(Schema.Primitive.Number.apply)(_.self)

      given Invariant[Schema.Primitive.Number] =
        Invariant[[a] =>> Annotation[PrimitiveBase.Number[a]]].imapK([A] =>
          (annotation: Annotation[PrimitiveBase.Number[A]]) => Schema.Primitive.Number(annotation)
        )([A] => (schema: Schema.Primitive.Number[A]) => schema.self)

    sealed abstract class Text[A]
        extends Schema.Primitive[A],
          Schema.Primitive.Text.Read[A],
          Schema.Primitive.Text.Write[A]:
      override def self: Annotation[PrimitiveBase.Text[A]]

    object Text:
      sealed trait Read[+A] extends Schema.Primitive.Read[A]:
        override def self: Annotation[PrimitiveBase.Text.Read[A]]

      object Read:
        def apply[A](annotation: Annotation[PrimitiveBase.Text.Read[A]]): Schema.Primitive.Text.Read[A] =
          new Schema.Primitive.Text.Read[A]:
            override def self: Annotation[PrimitiveBase.Text.Read[A]] = annotation

        def unapply[A](schema: Schema.Primitive.Text.Read[A]): Annotation[PrimitiveBase.Text.Read[A]] = schema.self

        given [A]: Annotated[Schema.Primitive.Text.Read[A]] =
          Annotated[Annotation[PrimitiveBase.Text.Read[A]]].imap(Schema.Primitive.Text.Read.apply)(_.self)

        given Functor[Schema.Primitive.Text.Read] =
          Functor[[a] =>> Annotation[PrimitiveBase.Text.Read[a]]].imapK([A] =>
            (annotation: Annotation[PrimitiveBase.Text.Read[A]]) => Schema.Primitive.Text.Read(annotation)
          )([A] => (schema: Schema.Primitive.Text.Read[A]) => schema.self)

      sealed trait Write[-A] extends Schema.Primitive.Write[A]:
        override def self: Annotation[PrimitiveBase.Text.Write[A]]

      object Write:
        def apply[A](annotation: Annotation[PrimitiveBase.Text.Write[A]]): Schema.Primitive.Text.Write[A] =
          new Schema.Primitive.Text.Write[A]:
            override def self: Annotation[PrimitiveBase.Text.Write[A]] = annotation

        def unapply[A](schema: Schema.Primitive.Text.Write[A]): Annotation[PrimitiveBase.Text.Write[A]] =
          schema.self

        given [A]: Annotated[Schema.Primitive.Text.Write[A]] =
          Annotated[Annotation[PrimitiveBase.Text.Write[A]]].imap(Schema.Primitive.Text.Write.apply)(_.self)

        given Contravariant[Schema.Primitive.Text.Write] =
          Contravariant[[a] =>> Annotation[PrimitiveBase.Text.Write[a]]].imapK([A] =>
            (annotation: Annotation[PrimitiveBase.Text.Write[A]]) => Schema.Primitive.Text.Write(annotation)
          )([A] => (schema: Schema.Primitive.Text.Write[A]) => schema.self)

      def apply[A](annotation: Annotation[PrimitiveBase.Text[A]]): Schema.Primitive.Text[A] =
        new Schema.Primitive.Text[A]:
          override def self: Annotation[PrimitiveBase.Text[A]] = annotation

      def unapply[A](schema: Schema.Primitive.Text[A]): Annotation[PrimitiveBase.Text[A]] = schema.self

      given [A]: Annotated[Schema.Primitive.Text[A]] =
        Annotated[Annotation[PrimitiveBase.Text[A]]].imap(Schema.Primitive.Text.apply)(_.self)

      given Invariant[Schema.Primitive.Text] =
        Invariant[[a] =>> Annotation[PrimitiveBase.Text[a]]].imapK([A] =>
          (annotation: Annotation[PrimitiveBase.Text[A]]) => Schema.Primitive.Text(annotation)
        )([A] => (schema: Schema.Primitive.Text[A]) => schema.self)

    def apply[A](annotation: Annotation[PrimitiveBase[A]]): Schema.Primitive[A] = annotation.self match
      case self: PrimitiveBase.Boolean[A] => Schema.Primitive.Boolean(annotation.copy(self = self))
      case self: PrimitiveBase.Number[A]  => Schema.Primitive.Number(annotation.copy(self = self))
      case self: PrimitiveBase.Text[A]    => Schema.Primitive.Text(annotation.copy(self = self))

    def unapply[A](schema: Schema.Primitive[A]): Annotation[PrimitiveBase[A]] = schema.self

    given Invariant[Schema.Primitive] =
      Invariant[[a] =>> Annotation[PrimitiveBase[a]]].imapK([A] =>
        (annotation: Annotation[PrimitiveBase[A]]) => Schema.Primitive(annotation)
      )([A] => (schema: Schema.Primitive[A]) => schema.self)

    given [A]: Annotated[Schema.Primitive[A]] =
      Annotated[Annotation[PrimitiveBase[A]]].imap(Schema.Primitive.apply)(_.self)

  sealed abstract class Tuple[+S[a] <: Schema[?, a], A]
      extends Schema[S, A],
        Schema.Tuple.Read[S, A],
        Schema.Tuple.Write[S, A]:
    override def self: Annotation[TupleBase[S, A]]

  object Tuple:
    sealed trait Read[+S[a] <: Schema.Read[?, a], +A] extends Schema.Read[S, A]:
      override def self: Annotation[TupleBase.Read[S, A]]

    object Read:
      def apply[S[a] <: Schema.Read[?, a], A](
          annotation: Annotation[TupleBase.Read[S, A]]
      ): Schema.Tuple.Read[S, A] = new Schema.Tuple.Read[S, A]:
        override def self: Annotation[TupleBase.Read[S, A]] = annotation

      def unapply[S[a] <: Schema.Read[?, a], A](
          schema: Schema.Tuple.Read[S, A]
      ): Annotation[TupleBase.Read[S, A]] = schema.self

      given [S[a] <: Schema.Read[?, a], A]: Annotated[Schema.Tuple.Read[S, A]] =
        Annotated[Annotation[TupleBase.Read[S, A]]].imap(Schema.Tuple.Read.apply)(_.self)

      given [S[a] <: Schema.Read[?, a]]: Functor[Schema.Tuple.Read[S, *]] =
        Functor[[a] =>> Annotation[TupleBase.Read[S, a]]].imapK([A] =>
          (annotation: Annotation[TupleBase.Read[S, A]]) => Schema.Tuple.Read(annotation)
        )([A] => (schema: Schema.Tuple.Read[S, A]) => schema.self)

      given Self.Tuple.Read[Schema.Tuple.Read, Schema.Read[?, *]] = Self.Tuple
        .Read[[s[a] <: Schema.Read[?, a], a] =>> Annotation[TupleBase.Read[s, a]], Schema.Read[?, *]]
        .imapK([s[a] <: Schema.Read[?, a], a] => (annotation: Annotation[TupleBase.Read[s, a]]) => Read(annotation))(
          [s[a] <: Schema.Read[?, a], a] => (schema: Schema.Tuple.Read[s, a]) => schema.self
        )

    sealed trait Write[+S[a] <: Schema.Write[?, a], -A] extends Schema.Write[S, A]:
      override def self: Annotation[TupleBase.Write[S, A]]

    object Write:
      def apply[S[a] <: Schema.Write[?, a], A](
          annotation: Annotation[TupleBase.Write[S, A]]
      ): Schema.Tuple.Write[S, A] = new Schema.Tuple.Write[S, A]:
        override def self: Annotation[TupleBase.Write[S, A]] = annotation

      def unapply[S[a] <: Schema.Write[?, a], A](
          schema: Schema.Tuple.Write[S, A]
      ): Annotation[TupleBase.Write[S, A]] = schema.self

      given [S[a] <: Schema.Write[?, a], A]: Annotated[Schema.Tuple.Write[S, A]] =
        Annotated[Annotation[TupleBase.Write[S, A]]].imap(Schema.Tuple.Write.apply)(_.self)

      given [S[a] <: Schema.Write[?, a]]: Contravariant[Schema.Tuple.Write[S, *]] =
        Contravariant[[a] =>> Annotation[TupleBase.Write[S, a]]].imapK([A] =>
          (annotation: Annotation[TupleBase.Write[S, A]]) => Schema.Tuple.Write(annotation)
        )([A] => (schema: Schema.Tuple.Write[S, A]) => schema.self)

      given Self.Tuple.Write[Schema.Tuple.Write, Schema.Write[?, *]] = Self.Tuple
        .Write[[s[a] <: Schema.Write[?, a], a] =>> Annotation[TupleBase.Write[s, a]], Schema.Write[?, *]]
        .imapK([s[a] <: Schema.Write[?, a], a] => (annotation: Annotation[TupleBase.Write[s, a]]) => Write(annotation))(
          [s[a] <: Schema.Write[?, a], a] => (schema: Schema.Tuple.Write[s, a]) => schema.self
        )

    def apply[S[a] <: Schema[?, a], A](annotation: Annotation[TupleBase[S, A]]): Schema.Tuple[S, A] =
      new Tuple[S, A]:
        override def self: Annotation[TupleBase[S, A]] = annotation

    def unapply[S[a] <: Schema[?, a], A](schema: Schema.Tuple[S, A]): Annotation[TupleBase[S, A]] =
      schema.self

    given [S[a] <: Schema[?, a], A]: Annotated[Schema.Tuple[S, A]] =
      Annotated[Annotation[TupleBase[S, A]]].imap(Schema.Tuple.apply)(_.self)

    given [S[a] <: Schema[?, a]]: Invariant[Schema.Tuple[S, *]] =
      Invariant[[a] =>> Annotation[TupleBase[S, a]]].imapK([A] =>
        (annotation: Annotation[TupleBase[S, A]]) => Schema.Tuple(annotation)
      )([A] => (schema: Schema.Tuple[S, A]) => schema.self)

    given Self.Tuple[Schema.Tuple, Schema[?, *]] = Self
      .Tuple[[s[a] <: Schema[?, a], a] =>> Annotation[TupleBase[s, a]], Schema[?, *]]
      .imapK([s[a] <: Schema[?, a], a] => (annotation: Annotation[TupleBase[s, a]]) => Tuple(annotation))(
        [s[a] <: Schema[?, a], a] => (schema: Tuple[s, a]) => schema.self
      )

  sealed abstract class Union[+S[a] <: Schema[?, a], A]
      extends Schema[S, A],
        Schema.Union.Read[S, A],
        Schema.Union.Write[S, A]:
    override def self: Annotation[UnionBase[Schema.Branch[S, *], A]]

  object Union:
    sealed trait Read[+S[a] <: Schema.Read[?, a], +A] extends Schema.Read[S, A]:
      override def self: Annotation[UnionBase.Read[Schema.Branch.Read[S, *], A]]

    object Read:
      def apply[S[a] <: Schema.Read[?, a], A](
          annotation: Annotation[UnionBase.Read[Schema.Branch.Read[S, *], A]]
      ): Schema.Union.Read[S, A] = new Schema.Union.Read[S, A]:
        override def self: Annotation[UnionBase.Read[Schema.Branch.Read[S, *], A]] = annotation

      def unapply[S[a] <: Schema.Read[?, a], A](
          schema: Schema.Union.Read[S, A]
      ): Annotation[UnionBase.Read[Schema.Branch.Read[S, *], A]] = schema.self

      given [S[a] <: Schema.Read[?, a], A]: Annotated[Schema.Union.Read[S, A]] =
        Annotated[Annotation[UnionBase.Read[Schema.Branch.Read[S, *], A]]].imap(Schema.Union.Read.apply)(_.self)

      given [S[a] <: Schema.Read[?, a]]: Functor[Schema.Union.Read[S, *]] =
        Functor[[a] =>> Annotation[UnionBase.Read[Schema.Branch.Read[S, *], a]]].imapK([A] =>
          (annotation: Annotation[UnionBase.Read[Schema.Branch.Read[S, *], A]]) => Schema.Union.Read(annotation)
        )([A] => (schema: Schema.Union.Read[S, A]) => schema.self)

      given Self.Union.Read[Schema.Union.Read, Schema.Branch.Read, Schema.Read[?, *]] = Self.Union
        .Read[
          [s[a] <: Schema.Read[?, a], a] =>> Annotation[UnionBase.Read[Schema.Branch.Read[s, *], a]],
          Schema.Branch.Read,
          Schema.Read[?, *]
        ]
        .imapK([s[a] <: Schema.Read[?, a], a] =>
          (annotation: Annotation[UnionBase.Read[Schema.Branch.Read[s, *], a]]) => Read(annotation)
        )([s[a] <: Schema.Read[?, a], a] => (schema: Read[s, a]) => schema.self)

    sealed trait Write[+S[a] <: Schema.Write[?, a], -A] extends Schema.Write[S, A]:
      override def self: Annotation[UnionBase.Write[Schema.Branch.Write[S, *], A]]

    object Write:
      def apply[S[a] <: Schema.Write[?, a], A](
          annotation: Annotation[UnionBase.Write[Schema.Branch.Write[S, *], A]]
      ): Schema.Union.Write[S, A] = new Schema.Union.Write[S, A]:
        override def self: Annotation[UnionBase.Write[Schema.Branch.Write[S, *], A]] = annotation

      def unapply[S[a] <: Schema.Write[?, a], A](
          schema: Schema.Union.Write[S, A]
      ): Annotation[UnionBase.Write[Schema.Branch.Write[S, *], A]] = schema.self

      given [S[a] <: Schema.Write[?, a], A]: Annotated[Schema.Union.Write[S, A]] =
        Annotated[Annotation[UnionBase.Write[Schema.Branch.Write[S, *], A]]].imap(Schema.Union.Write.apply)(_.self)

      given [S[a] <: Schema.Write[?, a]]: Contravariant[Schema.Union.Write[S, *]] =
        Contravariant[[a] =>> Annotation[UnionBase.Write[Schema.Branch.Write[S, *], a]]].imapK([A] =>
          (annotation: Annotation[UnionBase.Write[Schema.Branch.Write[S, *], A]]) => Schema.Union.Write(annotation)
        )([A] => (schema: Schema.Union.Write[S, A]) => schema.self)

      given Self.Union.Write[Schema.Union.Write, Schema.Branch.Write, Schema.Write[?, *]] = Self.Union
        .Write[
          [s[a] <: Schema.Write[?, a], a] =>> Annotation[UnionBase.Write[Schema.Branch.Write[s, *], a]],
          Schema.Branch.Write,
          Schema.Write[?, *]
        ]
        .imapK([s[a] <: Schema.Write[?, a], a] =>
          (annotation: Annotation[UnionBase.Write[Schema.Branch.Write[s, *], a]]) => Write(annotation)
        )([s[a] <: Schema.Write[?, a], a] => (schema: Write[s, a]) => schema.self)

    def apply[S[a] <: Schema[?, a], A](annotation: Annotation[UnionBase[Schema.Branch[S, *], A]]): Schema.Union[S, A] =
      new Union[S, A]:
        override def self: Annotation[UnionBase[Schema.Branch[S, *], A]] = annotation

    def unapply[S[a] <: Schema[?, a], A](schema: Schema.Union[S, A]): Annotation[UnionBase[Schema.Branch[S, *], A]] =
      schema.self

    given [S[a] <: Schema[?, a], A]: Annotated[Schema.Union[S, A]] =
      Annotated[Annotation[UnionBase[Schema.Branch[S, *], A]]].imap(Schema.Union.apply)(_.self)

    given [S[a] <: Schema[?, a]]: Invariant[Schema.Union[S, *]] =
      Invariant[[a] =>> Annotation[UnionBase[Schema.Branch[S, *], a]]].imapK([A] =>
        (annotation: Annotation[UnionBase[Schema.Branch[S, *], A]]) => Schema.Union(annotation)
      )([A] => (schema: Schema.Union[S, A]) => schema.self)

    given Self.Union[Schema.Union, Schema.Branch, Schema[?, *]] = Self
      .Union[
        [s[a] <: Schema[?, a], a] =>> Annotation[UnionBase[Schema.Branch[s, *], a]],
        Schema.Branch,
        Schema[?, *]
      ]
      .imapK([s[a] <: Schema[?, a], a] =>
        (annotation: Annotation[UnionBase[Schema.Branch[s, *], a]]) => Union(annotation)
      )([s[a] <: Schema[?, a], a] => (schema: Union[s, a]) => schema.self)

  sealed abstract class Field[+S[a] <: Schema[?, a], A] extends Schema.Field.Read[S, A], Schema.Field.Write[S, A]:
    override def self: Annotation[FieldBase[S, A]]

  object Field:
    sealed trait Read[+S[a] <: Schema.Read[?, a], +A]:
      def self: Annotation[FieldBase.Read[S, A]]

    object Read:
      def apply[S[a] <: Schema.Read[?, a], A](
          annotation: Annotation[FieldBase.Read[S, A]]
      ): Schema.Field.Read[S, A] = new Schema.Field.Read[S, A]:
        override def self: Annotation[FieldBase.Read[S, A]] = annotation

      def unapply[S[a] <: Schema.Read[?, a], A](schema: Schema.Field.Read[S, A]): Annotation[FieldBase.Read[S, A]] =
        schema.self

      given [S[a] <: Schema.Read[?, a], A]: Annotated[Schema.Field.Read[S, A]] =
        Annotated[Annotation[FieldBase.Read[S, A]]].imap(Schema.Field.Read.apply)(_.self)

      given [S[a] <: Schema.Read[?, a]]: Functor[Schema.Field.Read[S, *]] =
        Functor[[a] =>> Annotation[FieldBase.Read[S, a]]].imapK([A] =>
          (annotation: Annotation[FieldBase.Read[S, A]]) => Schema.Field.Read(annotation)
        )([A] => (schema: Schema.Field.Read[S, A]) => schema.self)

      given Self.Field.Read[Schema.Field.Read, Schema.Record.Read, Schema.Read[?, *]] = Self.Field
        .Read[
          [s[a] <: Schema.Read[?, a], a] =>> Annotation[FieldBase.Read[s, a]],
          Schema.Record.Read,
          Schema.Read[?, *]
        ]
        .imapK([s[a] <: Schema.Read[?, a], a] => (annotation: Annotation[FieldBase.Read[s, a]]) => Read(annotation))(
          [s[a] <: Schema.Read[?, a], a] => (schema: Schema.Field.Read[s, a]) => schema.self
        )

    sealed trait Write[+S[a] <: Schema.Write[?, a], -A]:
      def self: Annotation[FieldBase.Write[S, A]]

    object Write:
      def apply[S[a] <: Schema.Write[?, a], A](
          annotation: Annotation[FieldBase.Write[S, A]]
      ): Schema.Field.Write[S, A] = new Schema.Field.Write[S, A]:
        override def self: Annotation[FieldBase.Write[S, A]] = annotation

      def unapply[S[a] <: Schema.Write[?, a], A](
          schema: Schema.Field.Write[S, A]
      ): Annotation[FieldBase.Write[S, A]] = schema.self

      given [S[a] <: Schema.Write[?, a], A]: Annotated[Schema.Field.Write[S, A]] =
        Annotated[Annotation[FieldBase.Write[S, A]]].imap(Schema.Field.Write.apply)(_.self)

      given [S[a] <: Schema.Write[?, a]]: Contravariant[Schema.Field.Write[S, *]] =
        Contravariant[[a] =>> Annotation[FieldBase.Write[S, a]]].imapK([A] =>
          (annotation: Annotation[FieldBase.Write[S, A]]) => Schema.Field.Write(annotation)
        )([A] => (schema: Schema.Field.Write[S, A]) => schema.self)

      given Self.Field.Write[Schema.Field.Write, Schema.Record.Write, Schema.Write[?, *]] = Self.Field
        .Write[
          [s[a] <: Schema.Write[?, a], a] =>> Annotation[FieldBase.Write[s, a]],
          Schema.Record.Write,
          Schema.Write[?, *]
        ]
        .imapK([s[a] <: Schema.Write[?, a], a] => (annotation: Annotation[FieldBase.Write[s, a]]) => Write(annotation))(
          [s[a] <: Schema.Write[?, a], a] => (schema: Schema.Field.Write[s, a]) => schema.self
        )

    def apply[S[a] <: Schema[?, a], A](annotation: Annotation[FieldBase[S, A]]): Schema.Field[S, A] =
      new Schema.Field[S, A]:
        override def self: Annotation[FieldBase[S, A]] = annotation

    def unapply[S[a] <: Schema[?, a], A](schema: Schema.Field[S, A]): Annotation[FieldBase[S, A]] = schema.self

    given [S[a] <: Schema[?, a], A]: Annotated[Schema.Field[S, A]] =
      Annotated[Annotation[FieldBase[S, A]]].imap(Schema.Field.apply)(_.self)

    given [S[a] <: Schema[?, a]]: Invariant[Schema.Field[S, *]] =
      Invariant[[a] =>> Annotation[FieldBase[S, a]]].imapK([A] =>
        (annotation: Annotation[FieldBase[S, A]]) => Schema.Field(annotation)
      )([A] => (schema: Schema.Field[S, A]) => schema.self)

    given Self.Field[Schema.Field, Schema.Record, Schema[?, *]] = Self
      .Field[
        [s[a] <: Schema[?, a], a] =>> Annotation[FieldBase[s, a]],
        Schema.Record,
        Schema[?, *]
      ]
      .imapK([s[a] <: Schema[?, a], a] => (annotation: Annotation[FieldBase[s, a]]) => Field(annotation))(
        [s[a] <: Schema[?, a], a] => (schema: Schema.Field[s, a]) => schema.self
      )

  sealed abstract class Branch[+S[a] <: Schema[?, a], A] extends Schema.Branch.Read[S, A], Schema.Branch.Write[S, A]:
    override def self: Annotation[BranchBase[S, A]]

  object Branch:
    sealed trait Read[+S[a] <: Schema.Read[?, a], +A]:
      def self: Annotation[BranchBase.Read[S, A]]

    object Read:
      def apply[S[a] <: Schema.Read[?, a], A](
          annotation: Annotation[BranchBase.Read[S, A]]
      ): Schema.Branch.Read[S, A] = new Schema.Branch.Read[S, A]:
        override def self: Annotation[BranchBase.Read[S, A]] = annotation

      def unapply[S[a] <: Schema.Read[?, a], A](schema: Schema.Branch.Read[S, A]): Annotation[BranchBase.Read[S, A]] =
        schema.self

      given [S[a] <: Schema.Read[?, a], A]: Annotated[Schema.Branch.Read[S, A]] =
        Annotated[Annotation[BranchBase.Read[S, A]]].imap(Schema.Branch.Read.apply)(_.self)

      given [S[a] <: Schema.Read[?, a]]: Functor[Schema.Branch.Read[S, *]] =
        Functor[[a] =>> Annotation[BranchBase.Read[S, a]]].imapK([A] =>
          (annotation: Annotation[BranchBase.Read[S, A]]) => Schema.Branch.Read(annotation)
        )([A] => (schema: Schema.Branch.Read[S, A]) => schema.self)

      given Self.Branch.Read[Schema.Branch.Read, Schema.Union.Read, Schema.Read[?, *]] = Self.Branch
        .Read[
          [s[a] <: Schema.Read[?, a], a] =>> Annotation[BranchBase.Read[s, a]],
          Schema.Union.Read,
          Schema.Read[?, *]
        ]
        .imapK([s[a] <: Schema.Read[?, a], a] => (annotation: Annotation[BranchBase.Read[s, a]]) => Read(annotation))(
          [s[a] <: Schema.Read[?, a], a] => (schema: Schema.Branch.Read[s, a]) => schema.self
        )

    sealed trait Write[+S[a] <: Schema.Write[?, a], -A]:
      def self: Annotation[BranchBase.Write[S, A]]

    object Write:
      def apply[S[a] <: Schema.Write[?, a], A](
          annotation: Annotation[BranchBase.Write[S, A]]
      ): Schema.Branch.Write[S, A] = new Schema.Branch.Write[S, A]:
        override def self: Annotation[BranchBase.Write[S, A]] = annotation

      def unapply[S[a] <: Schema.Write[?, a], A](
          schema: Schema.Branch.Write[S, A]
      ): Annotation[BranchBase.Write[S, A]] = schema.self

      given [S[a] <: Schema.Write[?, a], A]: Annotated[Schema.Branch.Write[S, A]] =
        Annotated[Annotation[BranchBase.Write[S, A]]].imap(Schema.Branch.Write.apply)(_.self)

      given [S[a] <: Schema.Write[?, a]]: Contravariant[Schema.Branch.Write[S, *]] =
        Contravariant[[a] =>> Annotation[BranchBase.Write[S, a]]].imapK([A] =>
          (annotation: Annotation[BranchBase.Write[S, A]]) => Schema.Branch.Write(annotation)
        )([A] => (schema: Schema.Branch.Write[S, A]) => schema.self)

      given Self.Branch.Write[Schema.Branch.Write, Schema.Union.Write, Schema.Write[?, *]] = Self.Branch
        .Write[
          [s[a] <: Schema.Write[?, a], a] =>> Annotation[BranchBase.Write[s, a]],
          Schema.Union.Write,
          Schema.Write[?, *]
        ]
        .imapK([s[a] <: Schema.Write[?, a], a] =>
          (annotation: Annotation[BranchBase.Write[s, a]]) => Write(annotation)
        )([s[a] <: Schema.Write[?, a], a] => (schema: Schema.Branch.Write[s, a]) => schema.self)

    def apply[S[a] <: Schema[?, a], A](annotation: Annotation[BranchBase[S, A]]): Schema.Branch[S, A] =
      new Schema.Branch[S, A]:
        override def self: Annotation[BranchBase[S, A]] = annotation

    def unapply[S[a] <: Schema[?, a], A](schema: Schema.Branch[S, A]): Annotation[BranchBase[S, A]] = schema.self

    given [S[a] <: Schema[?, a], A]: Annotated[Schema.Branch[S, A]] =
      Annotated[Annotation[BranchBase[S, A]]].imap(Schema.Branch.apply)(_.self)

    given [S[a] <: Schema[?, a]]: Invariant[Schema.Branch[S, *]] =
      Invariant[[a] =>> Annotation[BranchBase[S, a]]].imapK([A] =>
        (annotation: Annotation[BranchBase[S, A]]) => Schema.Branch(annotation)
      )([A] => (schema: Schema.Branch[S, A]) => schema.self)

    given Self.Branch[Schema.Branch, Schema.Union, Schema[?, *]] = Self
      .Branch[
        [s[a] <: Schema[?, a], a] =>> Annotation[BranchBase[s, a]],
        Schema.Union,
        Schema[?, *]
      ]
      .imapK([s[a] <: Schema[?, a], a] => (annotation: Annotation[BranchBase[s, a]]) => Branch(annotation))(
        [s[a] <: Schema[?, a], a] => (schema: Schema.Branch[s, a]) => schema.self
      )

  def apply[S[a] <: Schema[?, a], A](annotation: Annotation[Schema.Of[S, A]]): Schema[S, A] = annotation.self match
    case self: CoerceBase[S, A]                  => Schema.Coerce(annotation.copy(self = self))
    case self: CollectionBase[S, A]              => Schema.Collection(annotation.copy(self = self))
    case self: ConstantBase[S, A]                => Schema.Constant(annotation.copy(self = self))
    case self: DictionaryBase[S, A]              => Schema.Dictionary(annotation.copy(self = self))
    case self: EnumerationBase[S, A]             => Schema.Enumeration(annotation.copy(self = self))
    case self: NullishBase[S, A]                 => Schema.Nullish(annotation.copy(self = self))
    case self: PrimitiveBase[A]                  => Schema.Primitive(annotation.copy(self = self))
    case self: RecordBase[Schema.Field[S, *], A] => Schema.Record(annotation.copy(self = self))
    case self: TupleBase[S, A]                   => Schema.Tuple(annotation.copy(self = self))
    case self: UnionBase[Schema.Branch[S, *], A] => Schema.Union(annotation.copy(self = self))

  def unapply[S[a] <: Schema[?, a], A](schema: Schema[S, A]): Annotation[Schema.Of[S, A]] = schema.self

  given [S[a] <: Schema[?, a]]: Invariant[Schema[S, *]] with
    override def imap[A, B](fa: Schema[S, A])(f: A => B)(g: B => A): Schema[S, B] = fa match
      case schema: Schema.Coerce[S, A]      => schema.imap(f)(g)
      case schema: Schema.Collection[S, A]  => schema.imap(f)(g)
      case schema: Schema.Constant[S, A]    => schema.imap(f)(g)
      case schema: Schema.Dictionary[S, A]  => schema.imap(f)(g)
      case schema: Schema.Enumeration[S, A] => schema.imap(f)(g)
      case schema: Schema.Nullish[S, A]     => schema.imap(f)(g)
      case schema: Schema.Primitive[A]      => schema.imap(f)(g)
      case schema: Schema.Record[S, A]      => schema.imap(f)(g)
      case schema: Schema.Tuple[S, A]       => schema.imap(f)(g)
      case schema: Schema.Union[S, A]       => schema.imap(f)(g)

  given [S[a] <: Schema[?, a], A]: Annotated[Schema[S, A]] =
    Annotated[Annotation[Schema.Of[S, A]]].imap(Schema.apply)(_.self)
