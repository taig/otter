package io.taig.otter

trait CollectionBuilder[F[+_], A, B] extends CollectionBuilder.Reader[F, A, B], CollectionBuilder.Writer[A, B]

object CollectionBuilder:
  trait Reader[F[+_], A, B]:
    def validation[C, D]: SchemaValidation[F, A, C, D, B]

  trait Writer[A, B]:
    def from(b: B): A
