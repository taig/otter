package io.taig.otter.openapi

final case class Annotation[+S, +M](self: S, metadata: M)
