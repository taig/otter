package io.taig.otter

import cats.Invariant
import io.taig.otter.validation.Validation

trait ValidationInvariant[F[+_], G[_]] extends Invariant[G]:
  extension [A](self: G[A])
    def ivalidate[V1, V2, B](validation: SchemaValidation[F, A, V1, V2, B])(f: B => A): G[B]
    final def ivalidate_[V1, V2](validation: SchemaValidation[F, A, V1, V2, Unit]): G[A] =
      ivalidate(validation.tap)(identity)

  override def imap[A, B](fa: G[A])(f: A => B)(g: B => A): G[B] = ivalidate(fa)(Validation.lift(f))(g)
