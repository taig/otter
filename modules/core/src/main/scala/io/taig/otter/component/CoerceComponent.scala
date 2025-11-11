package io.taig.otter.component

import io.taig.otter.Coerce
import io.taig.otter.Reference

trait CoerceComponent[F[+_[a] <: G[a], _], G[_]](using F: Coerce[F, G]):
  def coerce[H[a] <: G[a], A](schema: => H[A]): F[H, A] = F.coerce(schema = Reference.later(schema))
