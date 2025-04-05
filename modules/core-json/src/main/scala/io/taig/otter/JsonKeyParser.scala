package io.taig.otter

import io.taig.otter.Json.Key

import cats.data.Validated

object JsonKeyParser extends Parser[Json.Key]:
  override def apply[A](codec: Json.Key[A], value: String): Validated[Violations, A] = codec match
    case Json.Key.Constant(value)  => ???
    case Json.Key.Primitive(value) => ???
    case Json.Key.Union(value)     => ???

  def apply[A](codec: Primitive[A], value: String): Validated[Violations, A] = ???
