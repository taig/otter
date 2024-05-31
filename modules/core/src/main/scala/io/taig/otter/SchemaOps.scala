package io.taig.otter

trait SchemaOps[F[_], G[_]]:
  extension [A](self: F[A]) def optional: G[Option[A]]
