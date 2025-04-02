package io.taig.otter

import cats.Invariant

abstract class CodecInvariant[Self[_]] extends Invariant[Self]

object CodecInvariant:
  abstract class Nullable[Self[_], Optional[_]](using optional: OptionalInvariant[Optional, Self])
      extends CodecInvariant[Self]:
    extension [A](self: Self[A])
      final def nullable: Optional[Option[A]] = optional.nullable(codec = self)
      final def nullable(default: A): Optional[A] = optional.nullable(codec = self, default)
