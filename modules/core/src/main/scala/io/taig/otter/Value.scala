package io.taig.otter

import io.taig.otter as Self

sealed abstract class Value[A] extends Product with Serializable

object Value:
  final case class Collection[+S[a] <: Value[a], A](self: Self.Collection[S, A]) extends Value[A]
