package io.taig.otter

import io.taig.otter as Base
import cats.Invariant
import cats.Functor
import cats.Contravariant

trait Instances extends Types:
  given schemaIsomorphicOps: SchemaIsomorphicOps[Schema.Of, Schema.Of, Collection.Of] = ???
  given schemaReaderOps: SchemaReaderOps[Schema.Reader.Of, Schema.Reader.Of, Collection.Reader.Of] = ???
  given schemaWriterOps: SchemaWriterOps[Schema.Writer.Of, Schema.Writer.Of, Collection.Writer.Of] = ???

  given primitiveRequiredIsomorphicOps: PrimitiveIsomorphicOps[Primitive.Required, Primitive, Collection.Of] = ???
  given primitiveRequiredReaderOps
      : PrimitiveReaderOps[Primitive.Required.Reader, Primitive.Reader, Collection.Reader.Of] = ???
  given primitiveRequiredWriterOps
      : PrimitiveWriterOps[Primitive.Required.Writer, Primitive.Writer, Collection.Writer.Of] = ???

  given primitiveIsomorphicOps: PrimitiveIsomorphicOps[Primitive, Primitive, Collection.Of] = ???
  given primitiveReaderOps: PrimitiveReaderOps[Primitive.Reader, Primitive.Reader, Collection.Reader.Of] = ???
  given primitiveWriterOps: PrimitiveWriterOps[Primitive.Writer, Primitive.Writer, Collection.Writer.Of] = ???
