package io.taig.otter

import io.taig.otter as Plain

trait Types[Attributes[S <: Plain.Schema[?]]]:
  type Apply[S <: Plain.Schema[?]] = Metadata.Annotation[S, Attributes[S]]

  def apply[S <: Plain.Schema[?]](sa: S, metadata: Attributes[S]): Apply[S] =
    Metadata.Annotation(sa, Metadata(metadata)(apply(sa, _)))

  type Schema[A] = Apply[Plain.Schema[A]]

  type Primitive[A] = Apply[Plain.Primitive[A]]
  object Primitive:
    type Required[A] = Apply[Plain.Primitive.Required[A]]
    type Optional[A] = Apply[Plain.Primitive.Optional[A]]

  type Product[A] = Apply[Plain.Product[A]]
