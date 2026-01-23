package io.taig.otter.operation

import io.taig.otter.Reference

trait UnionableOperation[F[_], G[_]]:
  self =>

  extension [A](fa: F[A]) def toUnion: G[A]

  extension [A](fa: F[A]) def :+[B](branch: => F[B]): G[Either[A, B]]

object UnionableOperation:
  trait Read[F[_], G[_]] extends UnionableOperation[F, G]

  object Read:
    inline def apply[F[_], G[_]](using self: UnionableOperation.Read[F, G]): UnionableOperation.Read[F, G] = self

    def derived[F[_], G[_]](using UnionOperation.Read[G, F]): UnionableOperation.Read[F, G] =
      new UnionableOperation.Read[F, G]:
        extension [A](fa: F[A]) override def toUnion: G[A] = UnionOperation.Read[G, F].lift(branch = Reference.now(fa))

        extension [A](fa: F[A])
          override def :+[B](branch: => F[B]): G[Either[A, B]] =
            fa.toUnion :+ branch

  trait Write[F[_], G[_]] extends UnionableOperation[F, G]

  object Write:
    inline def apply[F[_], G[_]](using self: UnionableOperation.Write[F, G]): UnionableOperation.Write[F, G] = self

    def derived[F[_], G[_]](using UnionOperation.Write[G, F]): UnionableOperation.Write[F, G] =
      new UnionableOperation.Write[F, G]:
        extension [A](fa: F[A]) override def toUnion: G[A] = UnionOperation.Write[G, F].lift(branch = Reference.now(fa))

        extension [A](fa: F[A]) override def :+[B](branch: => F[B]): G[Either[A, B]] = fa.toUnion :+ branch

  inline def apply[F[_], G[_]](using self: UnionableOperation[F, G]): UnionableOperation[F, G] = self

  def derived[F[_], G[_]](using UnionOperation[G, F]): UnionableOperation[F, G] =
    new UnionableOperation[F, G]:
      extension [A](fa: F[A]) override def toUnion: G[A] = UnionOperation[G, F].lift(branch = Reference.now(fa))

      extension [A](fa: F[A]) override def :+[B](branch: => F[B]): G[Either[A, B]] = fa.toUnion :+ branch
