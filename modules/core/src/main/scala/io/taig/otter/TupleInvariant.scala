package io.taig.otter

import cats.data.Chain

trait TupleInvariant[F[_, _], Of] extends SchemaInvariant[F[Of, *], F[Of, *]]:
  def schemas[A](fa: F[Of, A]): Chain[Of]
