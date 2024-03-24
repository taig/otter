package io.taig.otter

import io.taig.otter

trait Types[C <: Context]:
  val context: C

  type Schema[A]

  type Primitive[A] = otter.Primitive[context.Primitive, A]
  object Primitive:
    type Required[A] = otter.Primitive.Required[context.Primitive, A]
    type Optional[A] = otter.Primitive.Optional[context.Primitive, A]

  type Product[A]
