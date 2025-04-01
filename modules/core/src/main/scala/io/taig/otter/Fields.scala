package io.taig.otter

abstract class Fields[S[_], T[_]](keys: Primitives.Strings[S]):
  def apply[A, B](name: A, key: S[A], value: T[B]): Field[S, T, B]

  final def apply[A](name: String, codec: T[A]): Field[S, T, A] =
    apply(name, key = keys.string, value = codec)
