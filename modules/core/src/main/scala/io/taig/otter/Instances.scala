package io.taig.otter

import io.taig.otter as Base

// What we need:
// - Invariant, Functor and Contravariant instances for each schema type
// - Functor and Contravaraint syntax should be available to Invariant instances
// - Additional ops depending on Schema type

trait Instances extends Types
// given schemaInvariant: SchemaInvariant[Schema.Of, Schema.Of, Union.Of] with
//   extension [A, B](self: Union.Of[A, B]) override def imap[C](f: B => C)(g: C => B): Union.Of[A, C] = self.imap(f)(g)

//   extension [A, B](self: Schema.Of[A, B])
//     override def union: Union.Of[self.type, B] = self.unionWith(metadata.union)
//     override def orElse[C](schema: Schema[C]): Union.Of[self.type | schema.type, Either[B, C]] = ???
//       // union.orElseWith((_, _) => metadata.union, schema.union)

// trait Instances extends Instances1:
//   given schemaInvariant[F[_, _], A](using F: SchemaIsomorphicOps[F, ?, ?, ?, ?]): Invariant[F[A, *]] = F.invariant[A]
//   given schemaReaderFunctor[F[_, _], A](using F: SchemaReaderOps[F, ?, ?, ?, ?]): Functor[F[A, *]] = F.functor[A]
//   given schemaWriterContravariant[F[_, _], A](using F: SchemaWriterOps[F, ?, ?, ?, ?]): Contravariant[F[A, *]] =
//     F.contravariant[A]

//   given collectionIsomporphicOps: CollectionIsomporphicOps[Collection.Of, Schema, Union.Of, Schema.Writer] = ???

//   given primitiveRequiredIsomorphicOps: PrimitiveIsomorphicOps[
//     Primitive.Required,
//     Primitive,
//     Schema,
//     Collection.Of,
//     Union.Of,
//     Schema.Writer
//   ] with
//     override def invariant[A]: Invariant[Primitive.Required] = new Invariant:
//       override def imap[A, B](fa: Primitive.Required[A])(f: A => B)(g: B => A): Primitive.Required[B] = fa.imap(f)(g)

//     extension [A, B](self: Primitive.Required[B])
//       override def collection: Collection.Of[self.type, Vector[B]] = self.collectionWith(metadata.collection)
//       override def imap[C](f: B => C)(g: C => B): Primitive.Required[C] = invariant[B].imap(self)(f)(g)
//       override def ivalidate[C, D, E](
//           validation: Validation[B, Constraint.Primitive[(Schema.Writer[C], C)], (Schema.Writer[D], D), E]
//       )(f: E => B): Primitive.Required[E] = self.ivalidate(validation)(f)
//       override def optional: Primitive[Option[B]] = self.optional
//       override def orElse[C](schema: Schema[C]): Union.Of[self.type | schema.type, Either[B, C]] =
//         self.orElseWith(metadata.union, schema)

// trait Instances1 extends Instances2:
//   given collectionReaderOps: CollectionReaderOps[Collection.Of, Schema.Reader, Union.Reader.Of, Schema.Writer] = ???

//   given collectionWriterOps: CollectionWriterOps[Collection.Of, Schema.Writer, Union.Writer.Of] = ???

//   given primitiveRequiredReaderOps: PrimitiveReaderOps[
//     Primitive.Required.Reader,
//     Primitive.Reader,
//     Schema.Reader,
//     Collection.Reader.Of,
//     Union.Reader.Of,
//     Schema.Writer
//   ] = ???

//   given primitiveRequiredWriterOps: PrimitiveWriterOps[
//     Primitive.Required.Writer,
//     Primitive.Writer,
//     Schema.Writer,
//     Collection.Writer.Of,
//     Union.Writer.Of
//   ] = ???

// trait Instances2 extends Instances3:
//   given primitiveIsomorphicOps: PrimitiveIsomorphicOps[
//     Primitive,
//     Primitive,
//     Schema,
//     Collection.Of,
//     Union.Writer.Of,
//     Schema.Writer
//   ] = ???

// trait Instances3 extends Instances4:
//   given primitiveReaderOps: PrimitiveReaderOps[
//     Primitive.Reader,
//     Primitive.Reader,
//     Schema.Reader,
//     Collection.Reader.Of,
//     Union.Reader.Of,
//     Schema.Writer
//   ] = ???

//   given primitiveWriterOps: PrimitiveWriterOps[
//     Primitive.Writer,
//     Primitive.Writer,
//     Schema.Writer,
//     Collection.Writer.Of,
//     Union.Writer.Of
//   ] = ???

// trait Instances4 extends Instances5:
//   given schemaIsomorphicOps: SchemaIsomorphicOps[Schema.Of, Schema.Of, Schema, Collection.Of, Union.Of] = ???

// trait Instances5 extends Types:
//   given schemaReaderOps
//       : SchemaReaderOps[Schema.Reader.Of, Schema.Reader.Of, Schema.Reader, Collection.Reader.Of, Union.Reader.Of] =
//     ???

//   given schemaWriterOps
//       : SchemaWriterOps[Schema.Writer.Of, Schema.Writer.Of, Schema.Writer, Collection.Writer.Of, Union.Writer.Of] =
//     ???
