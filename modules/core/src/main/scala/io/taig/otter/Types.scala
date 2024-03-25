package io.taig.otter

import io.taig.otter as Plain

trait Types[Of[S <: Plain.Schema[?]]]:
  self =>
  type Apply[S <: Plain.Schema[A], +A] = Metadata.Annotation[S, Of[S]]

  def apply[S <: Plain.Schema[A], A](sa: S, metadata: Of[S]): Apply[S, A] =
    Metadata.Annotation(sa, Metadata(metadata)(self.apply(sa, _)))

  type Schema[+A] = Apply[Plain.Schema[A], A]

  type Primitive[+A] = Apply[Plain.Primitive[A], A]
  object Primitive:
    type Required[+A] = Apply[Plain.Primitive.Required[A], A]
    type Optional[+A] = Apply[Plain.Primitive.Optional[A], A]

  type Product[+A] = Apply[Plain.Product[A], A]
