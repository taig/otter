package io.taig.otter

import io.taig.otter as Plain

trait Types:
  trait Metadatas:
    type Schema
    type Value <: Schema
  val metadata: Metadatas

  final type Schema[A] = Annotation[Plain.Schema[A], metadata.Schema]
  final type Value[A] = Annotation[Plain.Value[A], metadata.Value]

  type Primitive[A] <: Value[A]
  trait Primitives:
    type Required[A] <: Primitive[A]
  val Primitive: Primitives

  type Product[A] <: Schema[A]
