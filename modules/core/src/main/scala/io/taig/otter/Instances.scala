package io.taig.otter

trait Instances extends Types:
  given schemaOps: SchemaOps[Schema.Of, Schema.Of, Collection.Of, Tuple.Of]
  given schemaInvariant[A]: SchemaInvariant[Schema.Of[A, *]]

  given schemaReaderOps: SchemaOps[Schema.Reader.Of, Schema.Reader.Of, Collection.Reader.Of, Tuple.Reader.Of]
  given schemaReaderFunctor[A]: SchemaFunctor[Schema.Reader.Of[A, *]]

  given schemaWriterOps: SchemaOps[Schema.Writer.Of, Schema.Writer.Of, Collection.Writer.Of, Tuple.Writer.Of]
  given schemaWriterContravariant[A]: SchemaContravariant[Schema.Writer.Of[A, *]]

  given collectionOps: CollectionOps[Collection.Of, Tuple.Of, Schema.Any]
