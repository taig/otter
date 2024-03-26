package io.taig.otter

import io.taig.otter as Plain

trait Types extends Context:
  type Apply[+S[a] <: Plain.Schema[a], +M <: metadata.Schema, A] = Annotation[S[A], M]

  type Schema[A] = Apply[Plain.Schema, metadata.Schema, A]

  type Primitive[A] = Apply[Plain.Primitive, metadata.Primitive, A]

  object Primitive:
    type Required[A] = Apply[Plain.Primitive, metadata.Primitive, A]
    type Optional[A] = Apply[Plain.Primitive, metadata.Primitive, A]

  type Product[A] = Apply[Plain.Product, metadata.Product, A]

  object Product:
    type Of[S <: Plain.Schema[?], A] = Apply[Plain.Product.Of[S, *], metadata.Product, A]
