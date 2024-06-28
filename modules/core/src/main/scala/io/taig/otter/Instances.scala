package io.taig.otter

import io.taig.otter as Base
import cats.Invariant
import cats.Functor
import cats.Contravariant
import cats.Comonad

trait Instances extends Types:
  given schemaApplicativeComonad: ApplicativeComonad[container.Schema]

  given collectionApplicativeComonad: ApplicativeComonad[container.Collection]

  given primitiveApplicativeComonad: ApplicativeComonad[container.Primitive]

  given unionApplicativeComonad: ApplicativeComonad[container.Union]

  given schemaIsomporhicInvariant[A]: Invariant[Schema.Of[A, *]] with
    override def imap[B, C](fa: Schema.Of[A, B])(f: B => C)(g: C => B): Schema.Of[A, C] =
      schemaApplicativeComonad.map(fa)(_.imap(f)(g))

  given schemaReaderFunctor[A]: Functor[Schema.Reader.Of[A, *]] with
    override def map[B, C](fa: Schema.Reader.Of[A, B])(f: B => C): Schema.Reader.Of[A, C] =
      schemaApplicativeComonad.map(fa)(_.map(f))

  given schemaWriterContravariant[A]: Contravariant[Schema.Writer.Of[A, *]] with
    override def contramap[B, C](fa: Schema.Writer.Of[A, B])(f: C => B): Schema.Writer.Of[A, C] =
      schemaApplicativeComonad.map(fa)(_.contramap(f))

  given collectionIsomporhicInvariant[A]: Invariant[Collection.Of[A, *]] with
    override def imap[B, C](fa: Collection.Of[A, B])(f: B => C)(g: C => B): Collection.Of[A, C] =
      collectionApplicativeComonad.map(fa)(_.imap(f)(g))

  given collectionReaderFunctor[A]: Functor[Collection.Reader.Of[A, *]] with
    override def map[B, C](fa: Collection.Reader.Of[A, B])(f: B => C): Collection.Reader.Of[A, C] =
      collectionApplicativeComonad.map(fa)(_.map(f))

  given collectionWriterContravariant[A]: Contravariant[Collection.Writer.Of[A, *]] with
    override def contramap[B, C](fa: Collection.Writer.Of[A, B])(f: C => B): Collection.Writer.Of[A, C] =
      collectionApplicativeComonad.map(fa)(_.contramap(f))

  given primitiveIsomorphicInvariant: Invariant[Primitive] = ???

  given primitiveReaderFunctor: Functor[Primitive.Reader] = ???

  given primitiveWriterContravariant: Contravariant[Primitive.Writer] = ???

  given primitiveRequiredIsomorphicInvariant: Invariant[Primitive.Required] = ???

  given primitiveRequiredReaderFunctor: Functor[Primitive.Required.Reader] = ???

  given primitiveRequiredWriterContravariant: Contravariant[Primitive.Required.Writer] = ???
