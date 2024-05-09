package io.taig.otter

import cats.data.NonEmptyChain

trait TupleInvariant[F[_, _], Of] extends SchemaInvariant[F[Of, *], F[Of, *]]:
  def schemas[A](fa: F[Of, A]): NonEmptyChain[Of]
