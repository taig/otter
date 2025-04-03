package io.taig.otter

abstract class EnumerationInvariant[Self[_], Value[_]] extends CodecInvariant[Self]:
  def lift[A](codec: Enumeration[Value, A]): Self[A]
  def extract[A](codec: Self[A]): Enumeration[Value, A]

  extension [A](self: Self[A])
    final override def metadata: Metadata = extract(self).metadata
    final override def modifyMetadata(f: Metadata => Metadata): Self[A] = lift(extract(self).modifyMetadata(f))
    final override def imap[B](f: A => B)(g: B => A): Self[B] = lift(extract(self).imap(f)(g))
