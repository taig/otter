package io.taig.otter

import io.taig.otter as Plain

trait Types:
  trait Metadata:
    type Schema
    type Primitive <: Schema
    type Product <: Schema

  val metadata: Metadata

  final type Schema[A] = Annotation[Plain.Schema[A], metadata.Schema]
  final type Primitive[A] = Annotation[Plain.Primitive[A], metadata.Primitive]
  object Primitive:
    type Required[A] = Annotation[Plain.Primitive.Required[A], metadata.Primitive]
