package io.taig.otter.http

import cats.syntax.all.*

final case class Route[F[_], +S[_], A, B](endpoint: Endpoint[S, A, B], implementation: A => F[B]):
  def modifyEndpoint[S1[a] >: S[a]](f: Endpoint[S, A, B] => Endpoint[S1, A, B]): Route[F, S1, A, B] =
    copy(endpoint = f(endpoint))

  def modifyImplementation[G[_], C](f: (A => F[B]) => (A => G[B])): Route[G, S, A, B] =
    copy(implementation = f(implementation))
