package io.taig.otter.codec

import cats.data.Validated
import cats.syntax.all.*
import io.taig.otter.Violations

/** Reads a value described by the schema `F`. Only the read direction of the schema is produced, so a schema whose read
  * side is [[io.taig.otter.Void]] can never yield a value.
  */
trait Decoder[-F[-_, +_], T]:
  self =>

  def decode[R](fa: F[Nothing, R], t: T): Validated[Violations, R]

  def contramap[U](f: U => T): Decoder[F, U] = new Decoder[F, U]:
    override def decode[R](fa: F[Nothing, R], u: U): Validated[Violations, R] = self.decode(fa, f(u))

  def contramapK[G[-_, +_]](fK: [w, r] => G[w, r] => F[w, r]): Decoder[G, T] = new Decoder[G, T]:
    override def decode[R](ga: G[Nothing, R], t: T): Validated[Violations, R] = self.decode(fK(ga), t)

object Decoder:
  /** A decoder that consumes part of its input and reports what is left, so that a record can read its fields in one
    * pass instead of looking each of them up again.
    */
  trait Remaining[-F[-_, +_], T] extends Decoder[F, T]:
    self =>

    def decodeRemaining[R](fa: F[Nothing, R], t: T): Validated[Violations, (T, R)]

    final override def decode[R](fa: F[Nothing, R], t: T): Validated[Violations, R] =
      decodeRemaining(fa, t).map(_._2)

    final def verify(f: T => Option[Violations]): Decoder[F, T] = new Decoder[F, T]:
      override def decode[R](fa: F[Nothing, R], t: T): Validated[Violations, R] =
        self.decode(fa, t).andThen(f(t).toInvalid)

    override def contramapK[G[-_, +_]](fK: [w, r] => G[w, r] => F[w, r]): Decoder.Remaining[G, T] =
      new Decoder.Remaining[G, T]:
        override def decodeRemaining[R](ga: G[Nothing, R], t: T): Validated[Violations, (T, R)] =
          self.decodeRemaining(fK(ga), t)
