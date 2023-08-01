package io.taig.crock.http

import cats.data.Chain

opaque type Routes[F[_]] = Chain[Endpoint.Implementation[F, ?, ?]]

object Routes:
  extension [F[_]](self: Routes[F])
    def toChain: Chain[Endpoint.Implementation[F, ?, ?]] = self
    def find(request: Request): Option[Endpoint.Implementation[F, ?, ?]] =
      self.find(_.endpoint.input.matches(request))
    def :+(endpoint: Endpoint.Implementation[F, ?, ?]): Routes[F] = self :+ endpoint
    def +:(endpoint: Endpoint.Implementation[F, ?, ?]): Routes[F] = endpoint +: self
    def ++(routes: Routes[F]): Routes[F] = self ++ routes.toChain

  def fromChain[F[_]](endpoints: Chain[Endpoint.Implementation[F, ?, ?]]): Routes[F] = endpoints
  def fromSeq[F[_]](endpoints: Seq[Endpoint.Implementation[F, ?, ?]]): Routes[F] = fromChain(Chain.fromSeq(endpoints))
  def apply[F[_]](endpoints: Endpoint.Implementation[F, ?, ?]*): Routes[F] = fromSeq(endpoints)
  def one[F[_]](endpoint: Endpoint.Implementation[F, ?, ?]): Routes[F] = fromChain(Chain.one(endpoint))
