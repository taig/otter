package io.taig.otter

final case class Cofree[S[+a] <: Schema[a], A, M](self: S[A], tail: Metadata[S, A, M])
