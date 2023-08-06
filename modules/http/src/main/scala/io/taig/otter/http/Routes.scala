package io.taig.otter.http

import cats.data.{Chain, NonEmptyChain}
import cats.syntax.all.*
import io.taig.otter.http.syntax.*

opaque type Routes[F[_]] = Chain[Route[F, ?, ?]]

object Routes:
  extension [F[_]](self: Routes[F])
    def toChain: Chain[Route[F, ?, ?]] = self
    def toNec: Option[NonEmptyChain[Route[F, ?, ?]]] = NonEmptyChain.fromChain(self)
    def :+(endpoint: Route[F, ?, ?]): Routes[F] = self :+ endpoint
    def ++(routes: Routes[F]): Routes[F] = self ++ routes.toChain

    def find(method: Method, url: Http.Url): Option[Route[F, ?, ?]] =
      def isMatchingPath(remainders: Http.Path, path: Chain[Segment[?]]): Boolean =
        (remainders.uncons, path.uncons) match
          case (Some((x, xs)), Some((Segment.Static(y), ys))) => x === y && isMatchingPath(xs, ys)
          case (Some((x, xs)), Some((segment: Segment.Parameter[?], ys))) =>
            val isMatching = isMatchingPath(xs, ys)
            if segment.isOptional then isMatching || isMatchingPath(x +: xs, ys) else isMatching
          case (Some(_), None) | (None, Some(_)) => false
          case (None, None)                      => path.isEmpty

      def isMatchingQueries(remainders: Http.Queries, queries: Chain[Query[?]]): Boolean = queries
        .foldLeft((true, remainders)) {
          case ((false, remainders), _) => (false, remainders)
          case ((true, remainders), query) =>
            if query.isCollection then (true, remainders.removeAll(query.name))
            else if query.isOptional then (true, remainders.removeFirst(query.name))
            else
              remainders
                .firstWithRemainders(query.name)
                .fold((false, remainders)) { case (_, remainders) => (true, remainders) }
        }
        ._1

      toChain.find: route =>
        val input = route.endpoint.request
        method === input.method &&
        isMatchingPath(url.path, input.url.path.toChain) &&
        isMatchingQueries(url.queries, input.url.queries.toChain)

  extension [F[_]](self: Route[F, ?, ?]) def +:(routes: Routes[F]): Routes[F] = self +: routes

  def fromChain[F[_]](endpoints: Chain[Route[F, ?, ?]]): Routes[F] = endpoints
  def fromSeq[F[_]](endpoints: Seq[Route[F, ?, ?]]): Routes[F] = fromChain(Chain.fromSeq(endpoints))
  def apply[F[_]](endpoints: Route[F, ?, ?]*): Routes[F] = fromSeq(endpoints)
  def one[F[_]](endpoint: Route[F, ?, ?]): Routes[F] = fromChain(Chain.one(endpoint))
