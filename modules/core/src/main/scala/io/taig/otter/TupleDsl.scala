package io.taig.otter

trait TupleDsl[+Self[_], -Value[_]]:
  protected def fromTuple[A](self: Tuple[Value, A]): Self[A]

  final def TNil: Self[Unit] = fromTuple(Tuple.Empty(metadata = Metadata.Empty))

  extension [A](self: Value[A])
    final def toTuple: Self[A] = fromTuple(Tuple.Root(codec = Reference.now(self), metadata = Metadata.Empty))
