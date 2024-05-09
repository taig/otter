package io.taig.otter

import cats.data.NonEmptyChain

trait TupleFunctor[F[_, _], Of] extends SchemaFunctor[F[Of, *], F[Of, *]]:
  def schemas[A](fa: F[Of, A]): NonEmptyChain[Of]
