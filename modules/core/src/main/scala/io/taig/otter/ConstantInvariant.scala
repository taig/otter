package io.taig.otter

import cats.Eq

trait ConstantInvariant[Self[_], Value[_]] extends CodecInvariant[Self]:
  def apply[A: Eq](codec: => Value[A], value: A): Self[A]

object ConstantInvariant:
  def apply[Self[_], Value[_]](
      lift: [A] => (codec: Constant[Value, A]) => Self[A],
      extract: [A] => (codec: Self[A]) => Constant[Value, A]
  ): ConstantInvariant[Self, Value] =
    new ConstantInvariant[Self, Value]:
      override def apply[A: Eq](codec: => Value[A], value: A): Self[A] =
        lift(Constant.Root(codec = Reference.Constant(self = Reference.later(codec), value), metadata = Metadata.Empty))

      extension [A](self: Self[A])
        override def metadata: Metadata = extract(self).metadata
        override def modifyMetadata(f: Metadata => Metadata): Self[A] = lift(extract(self).modifyMetadata(f))
        override def imap[B](f: A => B)(g: B => A): Self[B] = lift(extract(self).imap(f)(g))
