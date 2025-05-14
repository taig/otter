package io.taig.otter

import cats.data.Validated

abstract class Decoder[S[_], T]:
  self =>

  def apply[A](schema: S[A], value: T): Validated[Violations, A]

  final def leftMap[B](f: Violations => Violations) = new Decoder[S, T]:
    override def apply[A](codec: S[A], value: T): Validated[Violations, A] =
      self.apply(codec, value).leftMap(f)
