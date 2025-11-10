package io.taig.otter.component

import io.taig.otter.Coerce

trait CoerceComponent[F[+_[a] <: G[a], _], G[_]](using Coerce[F, G])
