package io.taig.otter

abstract class ConstantInvariant[Self[_], Value[_]] extends CodecInvariant[Self]:
  def lift[A](codec: Constant[Value, A]): Self[A]
  def extract[A](codec: Self[A]): Constant[Value, A]

  extension [A](self: Self[A])
    final override def metadata: Metadata = extract(self).metadata
    final override def imap[B](f: A => B)(g: B => A): Self[B] = lift(extract(self).imap(f)(g))
