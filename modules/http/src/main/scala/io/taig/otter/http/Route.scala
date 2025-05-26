package io.taig.otter.http

import cats.syntax.all.*

final case class Route[F[_], +S[_, _], A, B](endpoint: S[A, B], implementation: A => F[B]):
  def modifyEndpoint[S1[a, b] >: S[a, b]](f: S1[A, B] => S1[A, B]): Route[F, S1, A, B] = copy(endpoint = f(endpoint))
