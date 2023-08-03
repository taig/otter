package io.taig.otter.http

import cats.data.{Chain, NonEmptyChain}
import cats.syntax.all.*
import io.taig.otter.http.syntax.*

opaque type Routes[F[_]] = Chain[Endpoint.Implementation[F, ?, ?]]

object Routes:
  extension [F[_]](self: Routes[F])
    def toChain: Chain[Endpoint.Implementation[F, ?, ?]] = self
    def toNec: Option[NonEmptyChain[Endpoint.Implementation[F, ?, ?]]] = NonEmptyChain.fromChain(self)
    def :+(endpoint: Endpoint.Implementation[F, ?, ?]): Routes[F] = self :+ endpoint
    def ++(routes: Routes[F]): Routes[F] = self ++ routes.toChain

    def filter(method: Method, path: Chain[String], queries: Chain[(String, String)]): Routes[F] =
      def isMatchingPath(remainders: Chain[String], path: Chain[Segment[?]]): Boolean =
        (remainders.uncons, path.uncons) match
          case (Some((x, xs)), Some((Segment.Static(y), ys))) => x === y && isMatchingPath(xs, ys)
          case (Some((x, xs)), Some((segment: Segment.Parameter[?], ys))) =>
            val isMatching = isMatchingPath(xs, ys)
            if segment.isOptional then isMatching || isMatchingPath(x +: xs, ys) else isMatching
          case (Some(_), None) | (None, Some(_)) => false
          case (None, None)                      => path.isEmpty

      def isMatchingQueries(remainders: Chain[(String, String)], queries: Chain[Query[?]]): Boolean = queries
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

      toChain.filter: route =>
        val input = route.endpoint.input
        method === input.method &&
        isMatchingPath(path, input.url.path.toChain) &&
        isMatchingQueries(queries, input.url.queries.toChain)

  extension [F[_]](self: Endpoint.Implementation[F, ?, ?]) def +:(routes: Routes[F]): Routes[F] = self +: routes

  def fromChain[F[_]](endpoints: Chain[Endpoint.Implementation[F, ?, ?]]): Routes[F] = endpoints
  def fromSeq[F[_]](endpoints: Seq[Endpoint.Implementation[F, ?, ?]]): Routes[F] = fromChain(Chain.fromSeq(endpoints))
  def apply[F[_]](endpoints: Endpoint.Implementation[F, ?, ?]*): Routes[F] = fromSeq(endpoints)
  def one[F[_]](endpoint: Endpoint.Implementation[F, ?, ?]): Routes[F] = fromChain(Chain.one(endpoint))
