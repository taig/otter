package io.taig.otter.http

import io.taig.otter.http as Self
import io.taig.otter.Enriched

trait Types:
  type Body[+S[_], A] = Enriched[Self.Body[S, *], A]

object Types extends Types
