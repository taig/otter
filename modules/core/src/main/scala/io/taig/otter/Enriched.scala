package io.taig.otter

final case class Enriched[S[_], A](self: S[A], metadata: Metadata)
