package io.taig.otter

import cats.syntax.all.*

sealed trait Optional[+F[_], A]:
  def extract: Extract[F, ?]

  final def optional: Optional[F, Option[A]] = Optional.Root(this)

object Optional:
  final case class Root[+F[_], A](self: Optional[F, A]) extends Optional[F, Option[A]]:
    export self.extract

final case class Required[+F[_], A](fa: F[A]) extends Optional[F, A]:
  override def extract: Extract[F, ?] = Extract(fa)
