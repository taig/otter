package io.taig.otter

sealed trait Optional[F[_], A]:
  final def optional: Optional[F, Option[A]] = Optional.Root(this)

object Optional:
  final case class Root[F[_], A](self: Optional[F, A]) extends Optional[F, Option[A]]

final case class Required[F[_], A](fa: F[A]) extends Optional[F, A]
