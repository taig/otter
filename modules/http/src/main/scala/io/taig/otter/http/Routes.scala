package io.taig.otter.http

import cats.data.Chain

opaque type Routes[F[_], +S[_], +T[_], +U[_]] = Chain[Route[F, S, T, U, ?, ?]]

object Routes:
  extension [F[_], S[_], T[_], U[_]](self: Routes[F, S, T, U])
    inline def toChain: Chain[Route[F, S, T, U, ?, ?]] = self
    def toSeq: Seq[Route[F, S, T, U, ?, ?]] = self.toList
    def :+(endpoint: Route[F, S, T, U, ?, ?]): Routes[F, S, T, U] = self :+ endpoint
    def ++(routes: Routes[F, S, T, U]): Routes[F, S, T, U] = self ++ routes.toChain

  extension [F[_], S[_], T[_], U[_]](self: Route[F, S, T, U, ?, ?])
    def +:(routes: Routes[F, S, T, U]): Routes[F, S, T, U] = self +: routes

  def fromChain[F[_], S[_], T[_], U[_]](endpoints: Chain[Route[F, S, T, U, ?, ?]]): Routes[F, S, T, U] = endpoints
  def fromSeq[F[_], S[_], T[_], U[_]](endpoints: Seq[Route[F, S, T, U, ?, ?]]): Routes[F, S, T, U] =
    fromChain(Chain.fromSeq(endpoints))
  def apply[F[_], S[_], T[_], U[_]](endpoints: Route[F, S, T, U, ?, ?]*): Routes[F, S, T, U] =
    fromSeq(endpoints)
  def one[F[_], S[_], T[_], U[_]](endpoint: Route[F, S, T, U, ?, ?]): Routes[F, S, T, U] =
    fromChain(Chain.one(endpoint))
