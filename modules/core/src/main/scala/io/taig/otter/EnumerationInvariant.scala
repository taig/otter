package io.taig.otter

trait EnumerationInvariant[Self[_], Value[_]] extends CodecInvariant[Self]

object EnumerationInvariant:
  def apply[Self[_], Value[_]](
      lift: [A] => (codec: Enumeration[Value, A]) => Self[A],
      extract: [A] => (codec: Self[A]) => Enumeration[Value, A]
  ): EnumerationInvariant[Self, Value] = new EnumerationInvariant[Self, Value]:
    extension [A](self: Self[A])
      override def metadata: Metadata = extract(self).metadata
      override def modifyMetadata(f: Metadata => Metadata): Self[A] = lift(extract(self).modifyMetadata(f))
      override def imap[B](f: A => B)(g: B => A): Self[B] = lift(extract(self).imap(f)(g))
