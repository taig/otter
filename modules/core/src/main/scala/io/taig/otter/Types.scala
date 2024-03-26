package io.taig.otter

import io.taig.otter as Plain

trait Types extends Context:
  type Schema[A]

  type Primitive[A] <: Schema[A]

  object Primitive:
    type Required[A] <: Primitive[A]
    type Optional[A] <: Primitive[A]

  type Product[A] <: Schema[A]

  object Product:
    type Of[S <: Plain.Schema[?], A] <: Schema.Of[S, A]
