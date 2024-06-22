package io.taig.otter

import io.taig.otter as Base
import cats.Comonad
import cats.Applicative
import cats.Functor
import cats.Contravariant

trait Instances extends Types:
  implicit def asSchema: Applicative[AsSchema] & Comonad[AsSchema]
  implicit def asCollection: Applicative[AsCollection] & Comonad[AsCollection]
  implicit def asTuple: Applicative[AsTuple] & Comonad[AsTuple]
  implicit def asPrimitive: Applicative[AsPrimitive] & Comonad[AsPrimitive]

  implicit def schemaInvariant[A]: SchemaInvariant[Schema.Of[A, *]] = ???

  implicit def schemaFunctorOps[A, B](schema: Schema.Of[A, B]): Functor.Ops[Schema.Of[A, *], B] = ???
  implicit def schemaContravariantOps[A, B](schema: Schema.Of[A, B]): Contravariant.Ops[Schema.Of[A, *], B] = ???

  implicit def schemaReaderFunctor[A]: SchemaFunctor[Schema.Reader.Of[A, *]] = ???

  implicit def schemaWriterContravariant[A]: SchemaContravariant[Schema.Writer.Of[A, *]] = ???

  // implicit val primitiveInvariant: SchemaInvariant[Primitive] = ???
  // implicit val primitiveFunctor: SchemaInvariant[Primitive.Reader] = ???
  // implicit val primitiveContravariant: SchemaContravariant[Primitive.Writer] = ???

  // implicit val primitiveRequiredInvariant: SchemaInvariant[Primitive.Required] = ???
  // implicit val primitiveRequiredFunctor: SchemaFunctor[Primitive.Required.Reader] = ???
  // implicit val primitiveRequiredContravariant: SchemaContravariant[Primitive.Required.Writer] = ???

  // implicit def collectionInvariant[A]: SchemaInvariant[Collection.Of[A, *]] = ???
  // implicit def collectionReaderFunctor[A]: SchemaFunctor[Collection.Reader.Of[A, *]] = ???
  // implicit def collectionWriterContravariant[A]: SchemaContravariant[Collection.Writer.Of[A, *]] = ???

  // implicit def tupleInvariant[A]: SchemaInvariant[Tuple.Of[A, *]] = ???
  // implicit def tupleReaderFunctor[A]: SchemaFunctor[Tuple.Reader.Of[A, *]] = ???
  // implicit def tupleWriterContravariant[A]: SchemaContravariant[Tuple.Writer.Of[A, *]] = ???
