package io.taig.otter.http.syntax

import io.taig.otter.http.operation.QueryOperation
import io.taig.otter.Append
import io.taig.otter.http.operation.QueriesOperation

trait QuerySyntax:
  extension [F[_], G[_], H[_], A](self: F[A])(using F: QueryOperation[F, G], H: QueriesOperation[H, F])
    def :*[B](query: F[B]): H[Append[A, B]] = ???
