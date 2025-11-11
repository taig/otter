package io.taig.otter

trait WrapperK[F[_], G[_]]:
  def extract[H](fa: F[H]): G[H]

  def inject[H](ga: G[H]): F[H]

object WrapperK:
  inline def apply[F[_], G[_]](using self: WrapperK[F, G]): WrapperK[F, G] = self
