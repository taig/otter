package io.taig.otter.http.operation

trait PathableOperation[F[_], G[_]]:
  def toPath[A](segment: F[A]): G[A]