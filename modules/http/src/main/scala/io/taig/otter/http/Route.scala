package io.taig.otter.http

import cats.syntax.all.*

final case class Route[F[_], +S[_], +T[_], +U[_], A, B](endpoint: Endpoint[S, T, U, A, B], implementation: A => F[B]):
  def modifyEndpoint[S1[a] >: S[a], T1[a] >: T[a], U1[a] >: U[a]](
      f: Endpoint[S, T, U, A, B] => Endpoint[S1, T1, U1, A, B]
  ): Route[F, S1, T1, U1, A, B] = copy(endpoint = f(endpoint))
