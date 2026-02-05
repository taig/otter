package io.taig.otter.http.operation

trait PathableOperation[F[_], G[_]]:
  def toPath[A](fa: F[A]): G[A]