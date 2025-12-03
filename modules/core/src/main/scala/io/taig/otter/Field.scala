package io.taig.otter

final case class Field[+S[_], A](name: String, schema: Reference[S, A]):
  def mapK[S1[a] >: S[a], T[_]](fK: [A] => S1[A] => T[A]): Field[T, A] =
    copy(schema = schema.mapK[S1, T](fK))
