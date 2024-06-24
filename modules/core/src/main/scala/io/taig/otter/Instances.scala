package io.taig.otter

import io.taig.otter as Base
import cats.Invariant
import cats.Functor
import cats.Contravariant

trait Instances extends Instances1:
  given collectionInvariant[A]: Invariant[Collection.Of[A, *]] with
    override def imap[B, C](fa: Collection.Of[A, B])(f: B => C)(g: C => B): Collection.Of[A, C] = fa.imap(f)(g)

  given collectionReaderFunctor[A]: Functor[Collection.Reader.Of[A, *]] with
    override def map[B, C](fa: Collection.Reader.Of[A, B])(f: B => C): Collection.Reader.Of[A, C] = fa.map(f)

  given collectionWriterContravariant[A]: Contravariant[Collection.Writer.Of[A, *]] with
    override def contramap[B, C](fa: Collection.Writer.Of[A, B])(f: C => B): Collection.Writer.Of[A, C] =
      fa.contramap(f)

  given primitiveRequiredInvariant: Invariant[Primitive.Required] with
    override def imap[A, B](fa: Primitive.Required[A])(f: A => B)(g: B => A): Primitive.Required[B] = fa.imap(f)(g)

  given primitiveRequiredReaderFunctor: Functor[Primitive.Required.Reader] with
    override def map[A, B](fa: Primitive.Required.Reader[A])(f: A => B): Primitive.Required.Reader[B] = fa.map(f)

  given primitiveRequiredWriterContravariant: Contravariant[Primitive.Required.Writer] with
    override def contramap[A, B](fa: Primitive.Required.Writer[A])(f: B => A): Primitive.Required.Writer[B] =
      fa.contramap(f)

trait Instances1 extends Instances2:
  given primitiveInvariant: Invariant[Primitive] with
    override def imap[A, B](fa: Primitive[A])(f: A => B)(g: B => A): Primitive[B] = fa.imap(f)(g)

  given primitiveReaderFunctor: Functor[Primitive.Reader] with
    override def map[A, B](fa: Primitive.Reader[A])(f: A => B): Primitive.Reader[B] = fa.map(f)

  given primitiveWriterContravariant: Contravariant[Primitive.Writer] with
    override def contramap[A, B](fa: Primitive.Writer[A])(f: B => A): Primitive.Writer[B] = fa.contramap(f)

trait Instances2 extends Types:
  given schemaInvariant[A]: Invariant[Schema.Of[A, *]] with
    override def imap[B, C](fa: Schema.Of[A, B])(f: B => C)(g: C => B): Schema.Of[A, C] = fa.imap(f)(g)

  given schemaReaderFunctor[A]: Functor[Schema.Reader.Of[A, *]] with
    override def map[B, C](fa: Schema.Reader.Of[A, B])(f: B => C): Schema.Reader.Of[A, C] = fa.map(f)

  given schemaWriterContravariant[A]: Contravariant[Schema.Writer.Of[A, *]] with
    override def contramap[B, C](fa: Schema.Writer.Of[A, B])(f: C => B): Schema.Writer.Of[A, C] = fa.contramap(f)
