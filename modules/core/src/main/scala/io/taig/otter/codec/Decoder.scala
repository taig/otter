package io.taig.otter.codec

import cats.data.Validated
import io.taig.otter.Violations

trait Decoder[S[_], T]:
  self =>

  def decode[A](schema: S[A], value: T): Validated[Violations, A]

  final def contramap[U](f: U => T): Decoder[S, U] = new Decoder[S, U]:
    override def decode[A](schema: S[A], value: U): Validated[Violations, A] =
      self.decode(schema, f(value))

  final def leftMap[B](f: Violations => Violations): Decoder[S, T] = new Decoder[S, T]:
    override def decode[A](codec: S[A], value: T): Validated[Violations, A] =
      self.decode(codec, value).leftMap(f)

object Decoder:
  trait Remainding[S[_], T] extends Decoder[S, T]:
    final override def decode[A](schema: S[A], value: T): Validated[Violations, A] =
      decodeRemainding(schema, value).map((_, a) => a)

    def decodeRemainding[A](schema: S[A], value: T): Validated[Violations, (T, A)]
