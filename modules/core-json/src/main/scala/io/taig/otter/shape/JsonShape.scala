package io.taig.otter.shape

import io.taig.otter as Self

trait JsonShape:
  type Json[A] = Self.Json[?, A]

  object Json:
    type Of[S[a] <: Self.Json[?, a], A] = Self.Json[S, A]

    type Field[A] = Self.Json.Field[?, A]

    object Field:
      type Of[S[a] <: Self.Json[?, a], A] = Self.Json.Field[S, A]

object JsonShape extends JsonShape