package io.taig.otter

import io.taig.otter as Base
import cats.data.Chain

object Plain extends Dsl:
  override type AsSchema[+A] = A
  override type AsCollection[+A] = A
  override type AsPrimitive[+A] = A
  override type AsTuple[+A] = A
  override type AsUnion[+A] = A

  override given schemaReaderOps: Base.SchemaOps[
    Schema.Reader.Of,
    Schema.Reader.Of,
    Collection.Reader.Of,
    Tuple.Reader.Of
  ] = ???

  override given schemaWriterOps: Base.SchemaOps[
    Schema.Writer.Of,
    Schema.Writer.Of,
    Collection.Writer.Of,
    Tuple.Writer.Of
  ] = ???

  override def primitive[A](tpe: Base.Type[A]): Primitive.Required[A] = Base.Primitive.Required.Root(tpe)

  override given schemaInvariant[A]: Base.SchemaInvariant[Schema.Of[A, *]] = ???

  override given collectionOps: Base.CollectionOps[[A, B] =>> Base.Collection[[A] =>> A, A, B], [A,
  B] =>> Base.Tuple[[A] =>> A, A, B], Base.Schema[[A] =>> A, ?, ?]] = ???

  override given schemaOps
      : Base.SchemaOps[[A, B] =>> Base.Schema[[A] =>> A, A, B], [A, B] =>> Base.Schema[[A] =>> A, A, B], [A,
      B] =>> Base.Collection[[A] =>> A, A, B], [A, B] =>> Base.Tuple[[A] =>> A, A, B]] = ???
