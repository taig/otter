package io.taig.otter.http

import io.taig.otter.Reference

sealed abstract class Url[+F[_], +G[_], A] extends Url.Read[F, G, A], Url.Write[F, G, A]:
  final def product[F1[a] >: F[a], G1[a] >: G[a], B](url: Url[F1, G1, B]): Url[F1, G1, (A, B)] = ???

object Url:
  sealed trait Read[+F[_], +G[_], +A]:
    def path: Reference[F, ?]

    final def product[F1[a] >: F[a], G1[a] >: G[a], B](url: Url.Read[F1, G1, B]): Url.Read[F1, G1, (A, B)] = ???

    def queries: Reference[G, ?]

  object Read:
    final case class Modify[F[_], G[_], A, B](self: Url.Read[F, G, A], f: A => B) extends Url.Read[F, G, B]:
      export self.{path, queries}

    final case class Product[F[_], G[_], A, B](left: Url.Read[F, G, A], right: Url.Read[F, G, B])
        extends Url.Read[F, G, (A, B)]:
      override def path: Reference[F, ?] = ???
      override def queries: Reference[G, ?] = ???

  sealed trait Write[+F[_], +G[_], -A]:
    def path: Reference[F, ?]

    final def product[F1[a] >: F[a], G1[a] >: G[a], B](url: Url.Write[F1, G1, B]): Url.Write[F1, G1, (A, B)] = ???

    def queries: Reference[G, ?]

  object Write:
    final case class Modify[F[_], G[_], A, B](self: Url.Write[F, G, A], f: B => A) extends Url.Write[F, G, B]:
      export self.{path, queries}
