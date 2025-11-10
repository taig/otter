package io.taig.otter.instance

import io.taig.otter.Coerce
import io.taig.otter as Self
import io.taig.otter.base as Base
import io.taig.otter.syntax.CatsSyntax.*
import io.taig.otter.shape.SchemaShape.*
import cats.Functor
import cats.syntax.all.*
import io.taig.otter.Annotated
import io.taig.otter.Annotation
import cats.Contravariant
import cats.Invariant
import io.taig.otter.Collection
import Self.Dictionary

trait SchemaInstances:
  given schemaInvariant[S[a] <: Schema[a]]: Invariant[Schema.Of[S, *]] with
    override def imap[A, B](fa: Schema.Of[S, A])(f: A => B)(g: B => A): Schema.Of[S, B] = fa match
      case schema: Schema.Collection.Of[S, A] => schema.imap(f)(g)
      case schema: Schema.Dictionary.Of[S, A] => schema.imap(f)(g)
      case schema: Schema.Primitive[A]        => schema.imap(f)(g)

  given schemaAnnotated[S[a] <: Schema[a], A]: Annotated[Schema.Of[S, A]] =
    Annotated[Annotation[Self.Schema.Of[S, A]]].imap {
      case Annotation(metadata, self: Base.Collection[S, A]) => Schema.Collection(Annotation(metadata, self))
      case Annotation(metadata, self: Base.Dictionary[S, A]) => Schema.Dictionary(Annotation(metadata, self))
      case Annotation(metadata, self: Base.Primitive[A])     => ???
    }(_.self)

  given schemaReadFunctor[S[a] <: Schema.Read[a]]: Functor[Schema.Read.Of[S, *]] with
    override def map[A, B](schema: Schema.Read.Of[S, A])(f: A => B): Schema.Read.Of[S, B] = schema match
      case schema: Schema.Collection.Read.Of[S, A] => schema.map(f)
      case schema: Schema.Dictionary.Read.Of[S, A] => schema.map(f)
      // case schema: Schema.Primitive.Read[A]     => ???

  given schemaReadAnnotated[S[a] <: Schema.Read[a], A]: Annotated[Schema.Read.Of[S, A]] =
    Annotated[Annotation[Self.Schema.Read.Of[S, A]]].imap {
      case Annotation(metadata, self: Base.Coerce.Read[S, A])     => Schema.Coerce.Read(Annotation(metadata, self))
      case Annotation(metadata, self: Base.Collection.Read[S, A]) =>
        Schema.Collection.Read(Annotation(metadata, self))
      case Annotation(metadata, self: Base.Dictionary.Read[S, A]) =>
        Schema.Dictionary.Read(Annotation(metadata, self))
      case Annotation(metadata, self: Base.Primitive.Read[A]) => ???
    }(_.self)

  given schemaWriteContravariant[S[a] <: Schema.Write[a]]: Contravariant[Schema.Write.Of[S, *]] with
    override def contramap[A, B](schema: Schema.Write.Of[S, A])(f: B => A): Schema.Write.Of[S, B] = schema match
      case schema: Schema.Collection.Write.Of[S, A] => schema.contramap(f)
      case schema: Schema.Dictionary.Write.Of[S, A] => schema.contramap(f)

  given schemaWriteAnnotated[S[a] <: Schema.Write[a], A]: Annotated[Schema.Write.Of[S, A]] =
    Annotated[Annotation[Self.Schema.Write.Of[S, A]]].imap {
      case Annotation(metadata, self: Base.Collection.Write[S, A]) =>
        Schema.Collection.Write(Annotation(metadata, self))
      case Annotation(metadata, self: Base.Dictionary.Write[S, A]) =>
        Schema.Dictionary.Write(Annotation(metadata, self))
      case Annotation(metadata, self: Base.Primitive.Write[A]) => ???
    }(_.self)

  given schemaCoerceInvariant[S[a] <: Schema[a]]: Invariant[Schema.Coerce.Of[S, *]] =
    Invariant[[a] =>> Annotation[Base.Coerce[S, a]]].imapK([A] =>
      (annotation: Annotation[Base.Coerce[S, A]]) => Schema.Coerce(annotation)
    )([A] => (schema: Schema.Coerce.Of[S, A]) => schema.self)

  given schemaCoerceAnnotated[S[a] <: Schema[a], A]: Annotated[Schema.Coerce.Of[S, A]] =
    Annotated[Annotation[Base.Coerce[S, A]]].imap(Schema.Coerce.apply)(_.self)

  given schemaCoerce[S[a] <: Schema[a]]: Coerce[Schema.Coerce.Of, S] =
    Coerce[[s[a] <: S[a], a] =>> Annotation[Base.Coerce[s, a]], S]
      .imapK([s[a] <: S[a], a] => (self: Annotation[Base.Coerce[s, a]]) => Schema.Coerce(self))(
        [s[a] <: Schema[a], a] => (schema: Schema.Coerce.Of[s, a]) => schema.self
      )

  given schemaCoerceReadFunctor[S[a] <: Schema.Read[a]]: Functor[Schema.Coerce.Read.Of[S, *]] =
    Functor[[a] =>> Annotation[Base.Coerce.Read[S, a]]].imapK([A] =>
      (annotation: Annotation[Base.Coerce.Read[S, A]]) => Schema.Coerce.Read(annotation)
    )([A] => (schema: Schema.Coerce.Read.Of[S, A]) => schema.self)

  given schemaCoerceReadAnnotated[S[a] <: Schema.Read[a], A]: Annotated[Schema.Coerce.Read.Of[S, A]] =
    Annotated[Annotation[Base.Coerce.Read[S, A]]].imap(Schema.Coerce.Read.apply)(_.self)

  given schemaCoerceRead[S[a] <: Schema.Read[a]]: Coerce.Read[Schema.Coerce.Read.Of, S] = Coerce
    .Read[[s[a] <: S[a], a] =>> Annotation[Base.Coerce.Read[s, a]], S]
    .imapK([s[a] <: S[a], a] => (self: Annotation[Base.Coerce.Read[s, a]]) => Schema.Coerce.Read(self))(
      [s[a] <: S[a], a] => (schema: Schema.Coerce.Read.Of[s, a]) => schema.self
    )

  given schemaCoerceWriteContravariant[S[a] <: Schema.Write[a]]: Contravariant[Schema.Coerce.Write.Of[S, *]] =
    Contravariant[[a] =>> Annotation[Base.Coerce.Write[S, a]]].imapK([A] =>
      (annotation: Annotation[Base.Coerce.Write[S, A]]) => Schema.Coerce.Write(annotation)
    )([A] => (schema: Schema.Coerce.Write.Of[S, A]) => schema.self)

  given schemaCoerceWriteAnnotated[S[a] <: Schema.Write[a], A]: Annotated[Schema.Coerce.Write.Of[S, A]] =
    Annotated[Annotation[Base.Coerce.Write[S, A]]].imap(Schema.Coerce.Write.apply)(_.self)

  given schemaCoerceWrite[S[a] <: Schema.Write[a]]: Coerce.Write[Schema.Coerce.Write.Of, S] = Coerce
    .Write[[s[a] <: S[a], a] =>> Annotation[Base.Coerce.Write[s, a]], S]
    .imapK([s[a] <: S[a], a] => (self: Annotation[Base.Coerce.Write[s, a]]) => Schema.Coerce.Write(self))(
      [s[a] <: S[a], a] => (schema: Schema.Coerce.Write.Of[s, a]) => schema.self
    )

  given schemaCollectionInvariant[S[a] <: Schema[a]]: Invariant[Schema.Collection.Of[S, *]] =
    Invariant[[a] =>> Annotation[Base.Collection[S, a]]].imapK([A] =>
      (annotation: Annotation[Base.Collection[S, A]]) => Schema.Collection(annotation)
    )([A] => (schema: Schema.Collection.Of[S, A]) => schema.self)

  given schemaCollectionAnnotated[S[a] <: Schema[a], A]: Annotated[Schema.Collection.Of[S, A]] =
    Annotated[Annotation[Base.Collection[S, A]]].imap(Schema.Collection.apply)(_.self)

  given schemaCollection[S[a] <: Schema[a]]: Collection[Schema.Collection.Of, S] =
    Collection[[s[a] <: S[a], a] =>> Annotation[Base.Collection[s, a]], S]
      .imapK([s[a] <: S[a], a] => (self: Annotation[Base.Collection[s, a]]) => Schema.Collection(self))(
        [s[a] <: S[a], a] => (schema: Schema.Collection.Of[s, a]) => schema.self
      )

  given schemaCollectionReadFunctor[S[a] <: Schema.Read[a]]: Functor[Schema.Collection.Read.Of[S, *]] =
    Functor[[a] =>> Annotation[Base.Collection.Read[S, a]]].imapK([A] =>
      (annotation: Annotation[Base.Collection.Read[S, A]]) => Schema.Collection.Read(annotation)
    )([A] => (schema: Schema.Collection.Read.Of[S, A]) => schema.self)

  given schemaCollectionReadAnnotated[S[a] <: Schema.Read[a], A]: Annotated[Schema.Collection.Read.Of[S, A]] =
    Annotated[Annotation[Base.Collection.Read[S, A]]].imap(Schema.Collection.Read.apply)(_.self)

  given schemaCollectionRead[S[a] <: Schema.Read[a]]: Collection.Read[Schema.Collection.Read.Of, S] = Collection
    .Read[[s[a] <: S[a], a] =>> Annotation[Base.Collection.Read[s, a]], S]
    .imapK([s[a] <: S[a], a] => (self: Annotation[Base.Collection.Read[s, a]]) => Schema.Collection.Read(self))(
      [s[a] <: S[a], a] => (schema: Schema.Collection.Read.Of[s, a]) => schema.self
    )

  given schemaCollectionWriteContravariant[S[a] <: Schema.Write[a]]: Contravariant[Schema.Collection.Write.Of[S, *]] =
    Contravariant[[a] =>> Annotation[Base.Collection.Write[S, a]]].imapK([A] =>
      (annotation: Annotation[Base.Collection.Write[S, A]]) => Schema.Collection.Write(annotation)
    )([A] => (schema: Schema.Collection.Write.Of[S, A]) => schema.self)

  given schemaCollectionWriteAnnotated[S[a] <: Schema.Write[a], A]: Annotated[Schema.Collection.Write.Of[S, A]] =
    Annotated[Annotation[Base.Collection.Write[S, A]]].imap(Schema.Collection.Write.apply)(_.self)

  given schemaCollectionWrite[S[a] <: Schema.Write[a]]: Collection.Write[Schema.Collection.Write.Of, S] = Collection
    .Write[[s[a] <: S[a], a] =>> Annotation[Base.Collection.Write[s, a]], S]
    .imapK([s[a] <: S[a], a] => (self: Annotation[Base.Collection.Write[s, a]]) => Schema.Collection.Write(self))(
      [s[a] <: S[a], a] => (schema: Schema.Collection.Write.Of[s, a]) => schema.self
    )

  given schemaDictionaryInvariant[S[a] <: Schema[a]]: Invariant[Schema.Dictionary.Of[S, *]] =
    Invariant[[a] =>> Annotation[Base.Dictionary[S, a]]].imapK([A] =>
      (annotation: Annotation[Base.Dictionary[S, A]]) => Schema.Dictionary(annotation)
    )([A] => (schema: Schema.Dictionary.Of[S, A]) => schema.self)

  given schemaDictionaryAnnotated[S[a] <: Schema[a], A]: Annotated[Schema.Dictionary.Of[S, A]] =
    Annotated[Annotation[Base.Dictionary[S, A]]].imap(Schema.Dictionary.apply)(_.self)

  given schemaDictionary[S[a] <: Schema[a]]: Dictionary[Schema.Dictionary.Of, S] =
    Dictionary[[s[a] <: S[a], a] =>> Annotation[Base.Dictionary[s, a]], S]
      .imapK([s[a] <: S[a], a] => (self: Annotation[Base.Dictionary[s, a]]) => Schema.Dictionary(self))(
        [s[a] <: S[a], a] => (schema: Schema.Dictionary.Of[s, a]) => schema.self
      )

  given schemaDictionaryReadFunctor[S[a] <: Schema.Read[a]]: Functor[Schema.Dictionary.Read.Of[S, *]] =
    Functor[[a] =>> Annotation[Base.Dictionary.Read[S, a]]].imapK([A] =>
      (annotation: Annotation[Base.Dictionary.Read[S, A]]) => Schema.Dictionary.Read(annotation)
    )([A] => (schema: Schema.Dictionary.Read.Of[S, A]) => schema.self)

  given schemaDictionaryReadAnnotated[S[a] <: Schema.Read[a], A]: Annotated[Schema.Dictionary.Read.Of[S, A]] =
    Annotated[Annotation[Base.Dictionary.Read[S, A]]].imap(Schema.Dictionary.Read.apply)(_.self)

  given schemaDictionaryRead[S[a] <: Schema.Read[a]]: Dictionary.Read[Schema.Dictionary.Read.Of, S] = Dictionary
    .Read[[s[a] <: S[a], a] =>> Annotation[Base.Dictionary.Read[s, a]], S]
    .imapK([s[a] <: S[a], a] => (self: Annotation[Base.Dictionary.Read[s, a]]) => Schema.Dictionary.Read(self))(
      [s[a] <: S[a], a] => (schema: Schema.Dictionary.Read.Of[s, a]) => schema.self
    )

  given schemaDictionaryWriteContravariant[S[a] <: Schema.Write[a]]: Contravariant[Schema.Dictionary.Write.Of[S, *]] =
    Contravariant[[a] =>> Annotation[Base.Dictionary.Write[S, a]]].imapK([A] =>
      (annotation: Annotation[Base.Dictionary.Write[S, A]]) => Schema.Dictionary.Write(annotation)
    )([A] => (schema: Schema.Dictionary.Write.Of[S, A]) => schema.self)

  given schemaDictionaryWriteAnnotated[S[a] <: Schema.Write[a], A]: Annotated[Schema.Dictionary.Write.Of[S, A]] =
    Annotated[Annotation[Base.Dictionary.Write[S, A]]].imap(Schema.Dictionary.Write.apply)(_.self)

  given schemaDictionaryWrite[S[a] <: Schema.Write[a]]: Dictionary.Write[Schema.Dictionary.Write.Of, S] = Dictionary
    .Write[[s[a] <: S[a], a] =>> Annotation[Base.Dictionary.Write[s, a]], S]
    .imapK([s[a] <: S[a], a] => (self: Annotation[Base.Dictionary.Write[s, a]]) => Schema.Dictionary.Write(self))(
      [s[a] <: S[a], a] => (schema: Schema.Dictionary.Write.Of[s, a]) => schema.self
    )

  given schemaPrimitiveInvariant: Invariant[Schema.Primitive] =
    Invariant[[a] =>> Annotation[Base.Primitive[a]]].imapK([A] =>
      (annotation: Annotation[Base.Primitive[A]]) => Schema.Primitive(annotation)
    )([A] => (schema: Schema.Primitive[A]) => schema.self)

object SchemaInstances extends SchemaInstances
