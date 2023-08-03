package io.taig.crock.http

import cats.data.Chain
import cats.syntax.all.*

trait HttpServer[F[_], R]:
  def start(routes: Routes[F]): F[Unit]

  final def findMatchingRoutes(
      method: Method,
      path: Chain[String],
      queries: Chain[(String, String)],
      routes: Routes[F]
  ): Routes[F] = routes.filter: route =>
    val input = route.endpoint.input
    method === input.method &&
    isMatchingPath(path, input.url.path.toChain) &&
    isMatchingQueries(queries, input.url.queries.toChain)

  final private def isMatchingPath(input: Chain[String], path: Chain[Segment[?]]): Boolean =
    (input.uncons, path.uncons) match
      case (Some((x, xs)), Some((Segment.Static(y), ys))) => x === y && isMatchingPath(xs, ys)
      case (Some((x, xs)), Some((segment: Segment.Parameter[?], ys))) =>
        val isMatching = isMatchingPath(xs, ys)
        if segment.isOptional then isMatching || isMatchingPath(x +: xs, ys) else isMatching
      case (Some(_), None) | (None, Some(_)) => false
      case (None, None)                      => path.isEmpty

  final private def isMatchingQueries(input: Chain[(String, String)], queries: Chain[Query[?]]): Boolean = queries
    .foldLeft((true, input)) {
      case ((false, remainders), _) => (false, remainders)
      case ((true, remainders), query) =>
        if query.isCollection then (true, removeAll(remainders, query.name))
        else if query.isOptional then (true, removeFirst(remainders, query.name))
        else
          getFirstWithRemainders(remainders, query.name)
            .fold((false, remainders)) { case (_, remainders) => (true, remainders) }
    }
    ._1

  final private def getFirst[A](fa: Chain[(String, A)], name: String): Option[A] =
    fa.collectFirst { case (`name`, value) => value }
  final private def removeAll[A](fa: Chain[(String, A)], name: String): Chain[(String, A)] = fa.filter:
    case (`name`, _) => false
    case _           => true
  final private def removeFirst[A](fa: Chain[(String, A)], name: String): Chain[(String, A)] =
    var removed = false
    val result = List.newBuilder[(String, A)]
    fa.iterator.foreach {
      case (`name`, _) if !removed => removed = true; ()
      case entry                   => result += entry
    }
    Chain.fromSeq(result.result())
  final private def getFirstWithRemainders[A](fa: Chain[(String, A)], name: String): Option[(A, Chain[(String, A)])] =
    getFirst(fa, name).tupleRight(removeFirst(fa, name))
