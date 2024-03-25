package io.taig.otter

import io.taig.otter as Plain

trait Types[Of[S <: Plain.Schema[?]]]:
  type Apply[S[a] <: Plain.Schema[a], A] = Metadata.Annotation[S[A], Of[S[A]]]

  def apply[S[a] <: Plain.Schema[a], A](sa: S[A], metadata: Of[S[A]]): Apply[S, A] =
    Metadata.Annotation(sa, Metadata(metadata)(apply(sa, _)))

  type Schema[A] = Apply[Plain.Schema, A]

  type Primitive[A] = Apply[Plain.Primitive, A]
  object Primitive:
    type Required[A] = Apply[Plain.Primitive.Required, A]
    type Optional[A] = Apply[Plain.Primitive.Optional, A]

  type Product[A] = Apply[Plain.Product, A]
