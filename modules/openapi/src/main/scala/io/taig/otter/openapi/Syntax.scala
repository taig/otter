package io.taig.otter.openapi

import io.taig.otter.http as Http

trait Syntax extends Http.Types:
  extension [A <: Codec[?]: Metadata.Ops](self: A)
    def description: Attribute.Optional[A, String] = Attribute.Optional(self, Keys.description)
    def summary: Attribute.Optional[A, String] = Attribute.Optional(self, Keys.summary)

  extension [A <: Primitive[?]: Metadata.Ops](self: A)
    def format: Attribute.Optional[A, String] = Attribute.Optional(self, Keys.format)

  extension [A, B](self: Endpoint[A, B])
    def description: Attribute.Optional[Endpoint[A, B], String] = Attribute.Optional(self, Keys.description)
    def operationId: Attribute.Optional[Endpoint[A, B], String] = Attribute.Optional(self, Keys.operationId)
    def summary: Attribute.Optional[Endpoint[A, B], String] = Attribute.Optional(self, Keys.summary)
    def tags: Attribute.Collection[Endpoint[A, B], String] = Attribute.Collection(self, Keys.tags)

object Syntax extends Syntax
