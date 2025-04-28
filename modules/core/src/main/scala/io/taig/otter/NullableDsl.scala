package io.taig.otter

trait NullableDsl[+Self[_], -Value[_]](using codec: Codec.Nullable[Self, Value]):
  self =>

  final def nullable[A](codec: => Value[A]): Self[Option[A]] = self.codec.nullable(codec)
  final def nullable[A](codec: => Value[A], default: A): Self[A] = self.codec.nullable(codec, default)
  final def void: Self[Unit] = self.codec.void
