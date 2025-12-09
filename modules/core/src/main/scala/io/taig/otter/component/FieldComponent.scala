package io.taig.otter.component

import io.taig.otter.Field

trait FieldComponent[F[+_[_], _], G[+_[a] <: H[a], _], H[_]](using F: Field[F, G, H])