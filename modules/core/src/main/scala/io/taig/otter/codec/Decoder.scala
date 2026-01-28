package io.taig.otter.codec

import cats.data.Validated
import io.taig.otter.Violations
import cats.syntax.all.*

trait Decoder[-F[_], T]:
  self =>

  def decode[A](fa: F[A], a: T): Validated[Violations, A]

  def contramap[U](f: U => T): Decoder[F, U] = new Decoder[F, U]:
    override def decode[A](fa: F[A], u: U): Validated[Violations, A] = self.decode(fa, f(u))

  def contramapK[G[_]](fK: [A] => G[A] => F[A]): Decoder[G, T] = new Decoder[G, T]:
    override def decode[A](ga: G[A], a: T): Validated[Violations, A] = self.decode(fK(ga), a)

object Decoder:
  trait Remaining[-F[_], T] extends Decoder[F, T]:
    self =>

    def decodeRemaining[A](fa: F[A], a: T): Validated[Violations, (T, A)]

    final override def decode[A](fa: F[A], a: T): Validated[Violations, A] =
      decodeRemaining(fa, a).map(_._2)

    final def verify(f: T => Option[Violations]): Decoder[F, T] = new Decoder[F, T]:
      override def decode[A](fa: F[A], t: T): Validated[Violations, A] =
        self.decode(fa, t).andThen(f(t).toInvalid)

    override def contramapK[G[_]](fK: [A] => G[A] => F[A]): Decoder.Remaining[G, T] =
      new Remaining[G, T]:
        override def decodeRemaining[A](ga: G[A], a: T): Validated[Violations, (T, A)] =
          self.decodeRemaining(fK(ga), a)
