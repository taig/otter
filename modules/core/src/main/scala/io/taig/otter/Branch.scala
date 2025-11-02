package io.taig.otter

import cats.Invariant

final case class Branch[+S[_], A](name: String, schema: Reference[S, A]):
  def mapK[S1[a] >: S[a], T[_]](fK: [A] => S1[A] => T[A]): Branch[T, A] =
    copy(schema = schema.mapK[S1, T](fK))
