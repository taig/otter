package io.taig.otter.codec

import cats.data.Validated
import io.taig.otter.Violations

trait Decoder[-F[_], T]:
  self =>

  def decode[A](fa: F[A], a: T): Validated[Violations, A]

  def contramapK[G[_]](fK: [A] => G[A] => F[A]): Decoder[G, T] = new Decoder[G, T]:
    override def decode[A](ga: G[A], a: T): Validated[Violations, A] = self.decode(fK(ga), a)

object Decoder:
  trait Remaining[-F[_], T] extends Decoder[F, T]:
    self =>

    def decodeRemaining[A](fa: F[A], a: T): Validated[Violations, (T, A)]

    final override def decode[A](fa: F[A], a: T): Validated[Violations, A] =
      decodeRemaining(fa, a).map(_._2)

    override def contramapK[G[_]](fK: [A] => G[A] => F[A]): Decoder.Remaining[G, T] =
      new Remaining[G, T]:
        override def decodeRemaining[A](ga: G[A], a: T): Validated[Violations, (T, A)] =
          self.decodeRemaining(fK(ga), a)
