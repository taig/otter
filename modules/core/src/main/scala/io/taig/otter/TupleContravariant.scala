package io.taig.otter

trait TupleContravariant[F[_, _], Of] extends TupleInvariant[F, Of], SchemaContravariant[F[Of, *], F[Of, *]]
