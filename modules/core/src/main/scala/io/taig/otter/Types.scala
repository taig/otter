package io.taig.otter

import io.taig.otter
import io.taig.hmap.HMap

trait Types[Of[S[+a] <: Schema[a]]]:
  self =>
  type Apply[S[+a] <: otter.Schema[a], A] = Cofree[S, A, Of[S]]

  def apply[S[+a] <: otter.Schema[a], A](sa: S[A], initial: HMap[Of[S]]): Apply[S, A] =
    Cofree(
      sa,
      new Metadata[S, A, Of[S]] {
        override def values: HMap[Of[S]] = initial

        override def set(values: HMap[Of[S]]): Cofree[S, A, Of[S]] = self.apply(sa, values)
      }
    )

  // type Schema[A] = Apply[otter.Schema, A]

  // type Primitive[A] = Apply[otter.Primitive, A]
  // object Primitive:
  //   type Required[A] = Apply[otter.Primitive.Required, A]
  //   type Optional[A] = Apply[otter.Primitive.Optional, A]

  // type Product[A] = Apply[otter.Product, A]
