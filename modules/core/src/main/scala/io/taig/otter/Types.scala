package io.taig.otter

import io.taig.otter as Plain

trait Types:
  trait Metadata:
    type Schema[+A]
    type Primitive[+A] <: Schema[A]

  val Metadata: Metadata

  final type Schema[A] = Annotation[Plain.Schema[A], Metadata.Schema]
  final type Primitive[A] = Annotation[Plain.Primitive[A], Metadata.Primitive]
  object Primitive:
    type Required[A] = Annotation[Plain.Primitive.Required[A], Metadata.Primitive]
