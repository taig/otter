package io.taig.otter

import io.taig.otter as Base
import cats.Invariant
import cats.Functor
import cats.Contravariant

trait Instances extends Types:
  given schemaIsomporhicInvariant[A](using F: Functor[container.Schema]): Invariant[Schema.Of[A, *]] with
    override def imap[B, C](fa: Schema.Of[A, B])(f: B => C)(g: C => B): Schema.Of[A, C] = F.map(fa)(_.imap(f)(g))

  given schemaReaderFunctor[A](using F: Functor[container.Schema]): Functor[Schema.Reader.Of[A, *]] with
    override def map[B, C](fa: Schema.Reader.Of[A, B])(f: B => C): Schema.Reader.Of[A, C] = F.map(fa)(_.map(f))

  given schemaWriterContravariant[A](using F: Functor[container.Schema]): Contravariant[Schema.Writer.Of[A, *]] with
    override def contramap[B, C](fa: Schema.Writer.Of[A, B])(f: C => B): Schema.Writer.Of[A, C] =
      F.map(fa)(_.contramap(f))

  given collectionIsomporhicInvariant[A](using F: Functor[container.Collection]): ValidationInvariant.Collection[Collection.Of[A, *]] = ???

  given collectionReaderFunctor[A](using F: Functor[container.Collection]): ValidationFunctor.Collection[Collection.Reader.Of[A, *]] = ???

  given collectionWriterContravariant[A](using F: Functor[container.Collection]): ValidationContravariant.Collection[Collection.Writer.Of[A, *]] = ???

  given primitiveIsomorphicInvariant: ValidationInvariant.Primitive[Primitive] = ???

  given primitiveReaderFunctor: ValidationFunctor.Primitive[Primitive.Reader] = ???

  given primitiveWriterContravariant: ValidationContravariant.Primitive[Primitive.Writer] = ???

  given primitiveRequiredIsomorphicInvariant: ValidationInvariant.Primitive[Primitive.Required] = ???

  given primitiveRequiredReaderFunctor: ValidationFunctor.Primitive[Primitive.Required.Reader] = ???

  given primitiveRequiredWriterContravariant: ValidationContravariant.Primitive[Primitive.Required.Writer] = ???

  given unionIsomorphicInvariant[A]: Invariant[Union.Of[A, *]] = ???

  given unionReaderFunctor[A]: Functor[Union.Reader.Of[A, *]] = ???

  given unionWriterContravariant[A]: Contravariant[Union.Writer.Of[A, *]] = ???
