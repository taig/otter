package io.taig.otter

abstract class OptionalInvariant[Self[_], Root[_]] extends CodecInvariant[Self]:
  def lift[A](codec: Optional[Root, A]): Self[A]

  final def nullable[A](codec: Root[A]): Self[Option[A]] =
    lift(Optional.Nullable(codec = Reference.now(codec), metadata = Metadata.Empty))
  final def nullable[A](codec: Root[A], default: A): Self[A] =
    lift(Optional.Default(codec = Reference.now(codec), default, metadata = Metadata.Empty))
