package io.taig.otter

final case class Annotation[+S, +M](self: S, metadata: M)
