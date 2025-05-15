package io.taig.otter.codec

import cats.data.Validated
import io.taig.otter.Violations

trait Decoder[S[_], T]:
  self =>

  def apply[A](schema: S[A], value: T): Validated[Violations, A]

  final def contramap[U](f: U => T): Decoder[S, U] = new Decoder[S, U]:
    override def apply[A](schema: S[A], value: U): Validated[Violations, A] =
      self(schema, f(value))

  final def leftMap[B](f: Violations => Violations): Decoder[S, T] = new Decoder[S, T]:
    override def apply[A](codec: S[A], value: T): Validated[Violations, A] =
      self(codec, value).leftMap(f)
