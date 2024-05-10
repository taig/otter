package io.taig.otter

trait Types:
  type Schema[+Of, A] <: SchemaReader[Of, A] & SchemaWriter[Of, A]
  type SchemaReader[+Of, +A]
  type SchemaWriter[+Of, -A]

  type Collection[+Of, A] <: CollectionReader[Of, A] & CollectionWriter[Of, A]
  type CollectionReader[+Of, +A]
  type CollectionWriter[+Of, -A]
