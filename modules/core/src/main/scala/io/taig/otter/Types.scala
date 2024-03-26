package io.taig.otter

import io.taig.otter as Plain

trait Types extends Context:
  type Schema[A] = Plain.Schema[metadata.Schema, A]

  type Primitive[A] = Plain.Primitive[metadata.Primitive, A]

  object Primitive:
    type Required[A] = Plain.Primitive[metadata.Primitive, A]
    type Optional[A] = Plain.Primitive[metadata.Primitive, A]

  type Product[A] = Plain.Product[metadata.Product, A]

  object Product:
    type Of[S <: Plain.Schema[?, ?], A] = Plain.Product[metadata.Product, A]
