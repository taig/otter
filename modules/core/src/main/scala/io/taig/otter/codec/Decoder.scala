package io.taig.otter.codec

import cats.data.Validated
import io.taig.otter.Violations

trait Decoder[-S[_], T]:
  self =>

  def decode[A](schema: S[A], value: T): Validated[Violations, A]

  def contramapK[U[_]](gK: [A] => U[A] => S[A]): Decoder[U, T] = new Decoder[U, T]:
    override def decode[A](schema: U[A], value: T): Validated[Violations, A] =
      self.decode(schema = gK(schema), value)

object Decoder:
  trait Remaining[-S[_], T] extends Decoder[S, T]:
    self =>

    def decodeRemaining[A](schema: S[A], value: T): Validated[Violations, (T, A)]

    final override def decode[A](schema: S[A], value: T): Validated[Violations, A] =
      decodeRemaining(schema, value).map((_, a) => a)

    override def contramapK[U[_]](gK: [A] => U[A] => S[A]): Decoder.Remaining[U, T] = new Remaining[U, T]:
      override def decodeRemaining[A](schema: U[A], value: T): Validated[Violations, (T, A)] =
        self.decodeRemaining(schema = gK(schema), value)
