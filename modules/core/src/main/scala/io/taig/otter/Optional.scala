package io.taig.otter

trait Optional[F[_], G[a] >: F[a]]:
  def optional[A](fa: F[A]): G[Option[A]]
