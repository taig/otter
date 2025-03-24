package io.taig.otter.http

import cats.data.Chain

opaque type Routes[F[_]] = Chain[Route[F, ?, ?]]

object Routes:
  extension [F[_]](self: Routes[F])
    inline def toChain: Chain[Route[F, ?, ?]] = self
    def toSeq: Seq[Route[F, ?, ?]] = self.toList
    def :+(endpoint: Route[F, ?, ?]): Routes[F] = self :+ endpoint
    def ++(routes: Routes[F]): Routes[F] = self ++ routes.toChain

    def find(method: Method, url: Http.Url): Option[Route[F, ?, ?]] =
      toChain.find(_.endpoint.request.matches(method, url))

    def modify(f: Route[F, ?, ?] => Route[F, ?, ?]): Routes[F] = toChain.map(f)

  extension [F[_]](self: Route[F, ?, ?]) def +:(routes: Routes[F]): Routes[F] = self +: routes

  def fromChain[F[_]](endpoints: Chain[Route[F, ?, ?]]): Routes[F] = endpoints
  def fromSeq[F[_]](endpoints: Seq[Route[F, ?, ?]]): Routes[F] = fromChain(Chain.fromSeq(endpoints))
  def apply[F[_]](endpoints: Route[F, ?, ?]*): Routes[F] = fromSeq(endpoints)
  def one[F[_]](endpoint: Route[F, ?, ?]): Routes[F] = fromChain(Chain.one(endpoint))
