package io.taig.otter.http.syntax

import io.taig.otter.http as Self
import io.taig.otter.Enrichment

trait TypesSyntax:
  type Body[+S[_], A] = Enrichment[Self.Body[S, *], A]

object TypesSyntax extends TypesSyntax
