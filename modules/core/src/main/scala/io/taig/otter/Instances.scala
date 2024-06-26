package io.taig.otter

import io.taig.otter as Base
import cats.Invariant
import cats.Functor
import cats.Contravariant
import io.taig.otter.validation.Validation

trait Instances extends Instances1:
  given schemaInvariant[F[_, _], A](using F: SchemaIsomorphicOps[F, ?, ?]): Invariant[F[A, *]] = F.invariant[A]
  given schemaReaderFunctor[F[_, _], A](using F: SchemaReaderOps[F, ?, ?]): Functor[F[A, *]] = F.functor[A]
  given schemaWriterContravariant[F[_, _], A](using F: SchemaWriterOps[F, ?, ?]): Contravariant[F[A, *]] =
    F.contravariant[A]

  given collectionIsomporphicOps: CollectionIsomporphicOps[Collection.Of, Schema.Writer] = ???

  given primitiveRequiredIsomorphicOps: PrimitiveIsomorphicOps[
    Primitive.Required,
    Primitive,
    Collection.Of,
    Schema.Writer
  ] with
    override def invariant[A]: Invariant[Primitive.Required] = new Invariant:
      override def imap[A, B](fa: Primitive.Required[A])(f: A => B)(g: B => A): Primitive.Required[B] = fa.imap(f)(g)

    extension [A, B](self: Primitive.Required[B])
      override def collection: Collection.Of[self.type, Vector[B]] = self.collectionWith(metadata.collection)
      override def optional: Primitive[Option[B]] = self.optional
      override def ivalidate[C, D, E](
          validation: Validation[B, Constraint.Primitive[(Schema.Writer[C], C)], (Schema.Writer[D], D), E]
      )(f: E => B): Primitive.Required[E] = self.ivalidate(validation)(f)

trait Instances1 extends Instances2:
  given collectionReaderOps: CollectionReaderOps[Collection.Of, Schema.Writer] = ???

  given collectionWriterOps: CollectionWriterOps[Collection.Of] = ???

  given primitiveRequiredReaderOps: PrimitiveReaderOps[
    Primitive.Required.Reader,
    Primitive.Reader,
    Collection.Reader.Of,
    Schema.Writer
  ] = ???

  given primitiveRequiredWriterOps: PrimitiveWriterOps[
    Primitive.Required.Writer,
    Primitive.Writer,
    Collection.Writer.Of
  ] = ???

trait Instances2 extends Instances3:
  given primitiveIsomorphicOps: PrimitiveIsomorphicOps[
    Primitive,
    Primitive,
    Collection.Of,
    Schema.Writer
  ] = ???

trait Instances3 extends Instances4:
  given primitiveReaderOps: PrimitiveReaderOps[
    Primitive.Reader,
    Primitive.Reader,
    Collection.Reader.Of,
    Schema.Writer
  ] = ???

  given primitiveWriterOps: PrimitiveWriterOps[Primitive.Writer, Primitive.Writer, Collection.Writer.Of] = ???

trait Instances4 extends Instances5:
  given schemaIsomorphicOps: SchemaIsomorphicOps[Schema.Of, Schema.Of, Collection.Of] = ???

trait Instances5 extends Types:
  given schemaReaderOps: SchemaReaderOps[Schema.Reader.Of, Schema.Reader.Of, Collection.Reader.Of] = ???

  given schemaWriterOps: SchemaWriterOps[Schema.Writer.Of, Schema.Writer.Of, Collection.Writer.Of] = ???
