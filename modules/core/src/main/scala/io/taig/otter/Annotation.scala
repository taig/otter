package io.taig.otter

import io.taig.otter as Plain

final case class Annotation[+S, +M](self: S, metadata: M)
