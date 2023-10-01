package io.taig.otter.openapi

import io.circe.Encoder
import io.circe.syntax.*
import io.taig.otter.openapi.syntax.*

final case class Extended[A](value: A, extensions: Extensions)

object Extended:
  given [A: Encoder.AsObject]: Encoder.AsObject[Extended[A]] = extended =>
    extended.value.asJsonObject ++ extended.extensions.asJsonObject

  given [A]: Conversion[A, Extended[A]] with
    override def apply(a: A): Extended[A] = a.withoutExtensions
