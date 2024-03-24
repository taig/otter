package io.taig.otter

import io.taig.otter

trait Types[Metadata[S[a] <: otter.Schema[a]]]:
  type Apply[S[a] <: otter.Schema[a], A] = otter.Schema.With[S, A, Metadata[S]]
  def apply[S[a] <: otter.Schema[a], A](self: S[A], value: Metadata[S]): Apply[S, A] = otter.Schema.With(self, value)

  type Schema[A] = Apply[otter.Schema, A]

  type Primitive[A] = Apply[otter.Primitive, A]
  object Primitive:
    type Required[A] = Apply[otter.Primitive.Required, A]
    type Optional[A] = Apply[otter.Primitive.Optional, A]

  type Product[A] = Apply[otter.Product, A]
