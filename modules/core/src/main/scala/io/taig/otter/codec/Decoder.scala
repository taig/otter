package io.taig.otter.codec

import cats.data.Validated
import io.taig.otter.Violations

trait Decoder[F[_], A]:
  self =>

  def decode[B](fb: F[B], a: A): Validated[Violations, B]

  def contramapK[G[_]](fK: [A] => G[A] => F[A]): Decoder[G, A] = new Decoder[G, A]:
    override def decode[B](gb: G[B], a: A): Validated[Violations, B] = self.decode(fK(gb), a)

object Decoder:
  trait Remaining[F[_], A] extends Decoder[F, A]:
    self =>

    def decodeRemaining[B](fb: F[B], a: A): Validated[Violations, (A, B)]

    final override def decode[B](fb: F[B], a: A): Validated[Violations, B] =
      decodeRemaining(fb, a).map(_._2)

    override def contramapK[G[_]](fK: [A] => G[A] => F[A]): Decoder.Remaining[G, A] =
      new Remaining[G, A]:
        override def decodeRemaining[B](gb: G[B], a: A): Validated[Violations, (A, B)] =
          self.decodeRemaining(fK(gb), a)
