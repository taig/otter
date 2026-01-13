package io.taig.otter.operation

import io.taig.otter.Reference

trait OptionalableOperation[F[_], G[_]]:
  extension [A](fa: F[A])
    def optional: G[Option[A]]

    def optional(default: => A): G[A]

object OptionalableOperation:
  trait Read[F[_], G[_]] extends OptionalableOperation[F, G]

  object Read:
    inline def apply[F[_], G[_]](using self: OptionalableOperation.Read[F, G]): OptionalableOperation.Read[F, G] = self

    def derived[F[_], G[_]](using
        OptionalOperation.Read[G, F]
    ): OptionalableOperation.Read[F, G] = new OptionalableOperation.Read[F, G]:
      extension [A](fa: F[A])
        override def optional: G[Option[A]] = OptionalOperation.Read[G, F].lift(schema = Reference.now(fa))

        override def optional(default: => A): G[A] =
          OptionalOperation.Read[G, F].lift(schema = Reference.now(fa), default)

  trait Write[F[_], G[_]] extends OptionalableOperation[F, G]

  object Write:
    inline def apply[F[_], G[_]](using self: OptionalableOperation.Write[F, G]): OptionalableOperation.Write[F, G] =
      self

    def derived[F[_], G[_]](using
        OptionalOperation.Write[G, F]
    ): OptionalableOperation.Write[F, G] = new OptionalableOperation.Write[F, G]:
      extension [A](fa: F[A])
        override def optional: G[Option[A]] = OptionalOperation.Write[G, F].lift(schema = Reference.now(fa))

        override def optional(default: => A): G[A] =
          OptionalOperation.Write[G, F].lift(schema = Reference.now(fa), default)

  inline def apply[F[_], G[_]](using self: OptionalableOperation[F, G]): OptionalableOperation[F, G] = self

  def derived[F[_], G[_]](using
      OptionalOperation[G, F]
  ): OptionalableOperation[F, G] = new OptionalableOperation[F, G]:
    extension [A](fa: F[A])
      override def optional: G[Option[A]] = OptionalOperation[G, F].lift(schema = Reference.now(fa))

      override def optional(default: => A): G[A] = OptionalOperation[G, F].lift(schema = Reference.now(fa), default)
