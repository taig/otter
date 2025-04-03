package io.taig.otter

abstract class OptionalInvariant[Self[_], Value[_]] extends CodecInvariant[Self]:
  def lift[A](codec: Optional[Value, A]): Self[A]
  def extract[A](self: Self[A]): Optional[Value, A]

  extension [A](self: Self[A])
    final override def metadata: Metadata = extract(self).metadata
    final override def imap[B](f: A => B)(g: B => A): Self[B] =
      lift(extract(self).imap(f)(g))

  final def nullable[A](codec: Value[A]): Self[Option[A]] =
    lift(Optional.Nullable(codec = Reference.now(codec), metadata = Metadata.Empty))
  final def nullable[A](codec: Value[A], default: A): Self[A] =
    lift(Optional.Default(codec = Reference.now(codec), default, metadata = Metadata.Empty))
