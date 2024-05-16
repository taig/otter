package io.taig.otter

import io.taig.otter as Base

sealed trait Schema[+S[_], A]

object Schema:
  final case class Root[S[_], A](data: S[A]) extends Schema[S, A]

  type Any[+S[+_], A] = Schema[Data[S, ?, *], A]
