package io.taig.otter

final case class Cofree[S[+a] <: Schema[a], A, M <: Singleton](self: S[A], tail: Metadata[S, A, M])
