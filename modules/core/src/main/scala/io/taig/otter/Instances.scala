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

  given primitiveOps: PrimitiveOps[Primitive, Primitive, Collection.Of, Tuple.Of]
  given primitiveInvariant: SchemaInvariant[Primitive]

  given primitiveReaderOps: PrimitiveOps[Primitive.Reader, Primitive.Reader, Collection.Reader.Of, Tuple.Reader.Of]
  given primitiveReaderFunctor: SchemaFunctor[Primitive.Reader]

  given primitiveWriterOps: PrimitiveOps[Primitive.Writer, Primitive.Writer, Collection.Writer.Of, Tuple.Writer.Of]
  given primitiveWriterContravariant: SchemaContravariant[Primitive.Writer]

  given primitiveRequiredOps: PrimitiveOps[Primitive.Required, Primitive, Collection.Of, Tuple.Of]
  given primitiveRequiredInvariant: SchemaInvariant[Primitive.Required]

  given primitiveRequiredReaderOps
      : PrimitiveOps[Primitive.Required.Reader, Primitive.Reader, Collection.Reader.Of, Tuple.Reader.Of]
  given primitiveRequiredReaderFunctor: SchemaFunctor[Primitive.Required.Reader]

  given primitiveRequiredWriterOps
      : PrimitiveOps[Primitive.Required.Writer, Primitive.Writer, Collection.Writer.Of, Tuple.Writer.Of]
  given primitiveRequiredWriterContravariant: SchemaContravariant[Primitive.Required.Writer]
