package io.taig.otter

trait TupleFunctor[F[_, _], Of] extends TupleInvariant[F, Of], SchemaFunctor[F[Of, *], F[Of, *]]
