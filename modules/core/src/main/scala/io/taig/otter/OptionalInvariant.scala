package io.taig.otter

trait OptionalInvariant[Self[_], Value[_]] extends CodecInvariant[Self]:
  def nullable[A](codec: Value[A]): Self[Option[A]]
  def nullable[A](codec: Value[A], default: A): Self[A]

object OptionalInvariant:
  def apply[Self[_], Value[_]](
      lift: [A] => Optional[Value, A] => Self[A],
      extract: [A] => Self[A] => Optional[Value, A]
  ): OptionalInvariant[Self, Value] = new OptionalInvariant[Self, Value]:
    override def nullable[A](codec: Value[A]): Self[Option[A]] =
      lift(Optional.Nullable(codec = Reference.now(codec), metadata = Metadata.Empty))
    override def nullable[A](codec: Value[A], default: A): Self[A] =
      lift(Optional.Default(codec = Reference.now(codec), default, metadata = Metadata.Empty))

    extension [A](self: Self[A])
      override def metadata: Metadata = extract(self).metadata
      override def modifyMetadata(f: Metadata => Metadata): Self[A] =
        lift(extract(self).modifyMetadata(f))
      override def imap[B](f: A => B)(g: B => A): Self[B] = lift(extract(self).imap(f)(g))
