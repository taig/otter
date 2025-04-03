package io.taig.otter

import cats.Invariant

abstract class CodecInvariant[Self[_]]:
  self =>

  extension [A](self: Self[A])
    def imap[B](f: A => B)(g: B => A): Self[B]
    def metadata: Metadata
    def modifyMetadata(f: Metadata => Metadata): Self[A]

  final def invariant: Invariant[Self] = new Invariant[Self]:
    override def imap[A, B](fa: Self[A])(f: A => B)(g: B => A): Self[B] = self.imap(fa)(f)(g)

object CodecInvariant:
  abstract class Nullable[Self[_], Optional[_]](using optional: OptionalInvariant[Optional, Self])
      extends CodecInvariant[Self]:
    extension [A](self: Self[A])
      final def nullable: Optional[Option[A]] = optional.nullable(codec = self)
      final def nullable(default: A): Optional[A] = optional.nullable(codec = self, default)
