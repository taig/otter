package io.taig.otter.component

import io.taig.enumeration.ext.Mapping
import io.taig.otter.Enumeration
import io.taig.otter.Reference

trait EnumerationComponent[F[+_[a] <: G[a], _], G[_]](using F: Enumeration[F, G]):
  def enumeration[H[a] <: G[a], A, B](schema: => H[A], mapping: Mapping[B, A]): F[H, B] =
    F.enumeration(schema = Reference.later(schema), mapping)
