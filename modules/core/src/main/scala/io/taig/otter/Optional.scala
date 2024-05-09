package io.taig.otter

trait Optional[F[_]]:
  def optional[A](fa: F[A]): F[Option[A]]
