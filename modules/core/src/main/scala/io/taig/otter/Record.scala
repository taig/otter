package io.taig.otter
import cats.data.Chain

trait Record[F[+_[a] <: H[a], _], G[+_[a] <: H[a], _], H[_]]:
  self =>

  def empty: F[H, Unit]

  def record[I[a] <: H[a], A](field: Reference[G[I, *], A]): F[I, A]

  extension [A](fha: F[H, A])
    def fields: Chain[Reference[H, ?]]

    // def zip[B](schema: F[H, B]): F[H, (A, B)]

object Record:
  inline def apply[F[+_[a] <: H[a], _], G[+_[a] <: H[a], _], H[_]](using self: Record[F, G, H]): Record[F, G, H] = self
