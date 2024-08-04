package io.taig.otter.openapi

import io.taig.otter.Data
import io.taig.otter.Codec
import io.taig.otter.Attribute
import io.taig.otter.Metadata
import io.taig.otter.Primitive

trait Syntax:
  extension [A <: Codec[?, ?, ?]: Metadata.Ops](self: A)
    def description: Attribute.Optional[A, String] = Attribute.Optional(self, Keys.description)

  extension [F[+a] <: Data.Optional[a], A](self: Primitive[F, A])
    def format: Attribute.Optional[Primitive[F, A], String] = Attribute.Optional(self, Keys.format)

object Syntax extends Syntax
