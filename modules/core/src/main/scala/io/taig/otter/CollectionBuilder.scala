package io.taig.otter

trait CollectionBuilder[F[+_], A, B] extends CollectionBuilder.Reader[F, A, B], CollectionBuilder.Writer[A, B]

object CollectionBuilder:
  trait Reader[F[+_], A, B]:
    def validation: SchemaValidation[F, A, Nothing, Nothing, B]

  trait Writer[A, B]:
    def from(b: B): A
