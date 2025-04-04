package io.taig.otter

import cats.Eq

abstract class ConstantInvariant[Self[_], Value[_]] extends CodecInvariant[Self]:
  def lift[A](codec: Constant[Value, A]): Self[A]
  def extract[A](codec: Self[A]): Constant[Value, A]

  final def apply[A: Eq](codec: => Value[A], value: A): Self[A] =
    lift(Constant.Root(codec = Reference.Constant(self = Reference.later(codec), value), metadata = Metadata.Empty))

  extension [A](self: Self[A])
    final override def metadata: Metadata = extract(self).metadata
    final override def modifyMetadata(f: Metadata => Metadata): Self[A] = lift(extract(self).modifyMetadata(f))
    final override def imap[B](f: A => B)(g: B => A): Self[B] = lift(extract(self).imap(f)(g))
