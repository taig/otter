package io.taig.otter

trait OptionalDsl[+Self[_], -Value[_]]:
  protected def fromOptional[A](self: Optional[Value, A]): Self[A]

  extension [A](self: Value[A])
    final def nullable: Self[Option[A]] = fromOptional(
      Optional.Nullable(codec = Reference.now(self), metadata = Metadata.Empty)
    )
    final def nullable(default: A): Self[A] = fromOptional(
      Optional.Default(codec = Reference.now(self), default, metadata = Metadata.Empty)
    )
