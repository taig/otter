package io.taig.otter.http.syntax

import io.taig.otter.http as Self
import io.taig.otter.Enriched

trait TypesSyntax:
  type Body[+S[_], A] = Enriched[Self.Body[S, *], A]

object TypesSyntax extends TypesSyntax
