package io.taig.otter

trait TupleDsl[+Self[_], -Value[_]](using codec: Codec.Tuple[Self, Value]):
  final def TNil: Self[Unit] = codec.empty

  extension [A](self: Value[A]) final def toTuple: Self[A] = codec.one(self)
