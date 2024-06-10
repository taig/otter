package io.taig.otter

trait Instances extends Types:
  given schemaOps: SchemaOps[Schema.Of, Schema.Of, Collection.Of, Tuple.Of]
  given schemaInvariant[A]: SchemaInvariant[Schema.Of[A, *]]

  given schemaReaderOps: SchemaOps[Schema.Reader.Of, Schema.Reader.Of, Collection.Reader.Of, Tuple.Reader.Of]
  given schemaReaderFunctor[A]: SchemaFunctor[Schema.Reader.Of[A, *]]

  given schemaWriterOps: SchemaOps[Schema.Writer.Of, Schema.Writer.Of, Collection.Writer.Of, Tuple.Writer.Of]
  given schemaWriterContravariant[A]: SchemaContravariant[Schema.Writer.Of[A, *]]

  given collectionOps: CollectionOps[Collection.Of, Tuple.Of, Schema.Any]
  given collectionInvariant[A]: SchemaInvariant[Collection.Of[A, *]]

  given collectionReaderOps: CollectionOps[Collection.Reader.Of, Tuple.Reader.Of, Schema.Reader.Any]
  given collectionReaderFunctor[A]: SchemaFunctor[Collection.Reader.Of[A, *]]

  given collectionWriterOps: CollectionOps[Collection.Writer.Of, Tuple.Writer.Of, Schema.Writer.Any]
  given collectionWriterContravariant[A]: SchemaContravariant[Collection.Writer.Of[A, *]]

  given primitiveOps[F[a] <: Primitive[a]]: PrimitiveOps[F, Primitive, Collection.Of, Tuple.Of]
  given primitiveInvariant[A]: SchemaInvariant[Primitive]

  given primitiveReaderOps[F[a] <: Primitive.Reader[a]]
      : PrimitiveOps[F, Primitive.Reader, Collection.Reader.Of, Tuple.Reader.Of]
  given primitiveReaderFunctor[A]: SchemaFunctor[Primitive.Reader]

  given primitiveWriterOps[F[a] <: Primitive.Writer[a]]
      : PrimitiveOps[F, Primitive.Writer, Collection.Writer.Of, Tuple.Writer.Of]
  given primitiveWriterContravariant[A]: SchemaContravariant[Primitive.Writer]
