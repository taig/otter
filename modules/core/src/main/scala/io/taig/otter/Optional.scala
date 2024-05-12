package io.taig.otter

trait Optional[F[_], G[a] >: F[a]]:
  extension [A](fa: F[A]) def optional: G[Option[A]]
