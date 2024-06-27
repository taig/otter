package io.taig.otter.openapi

import cats.Functor
import cats.syntax.all.*

final case class Annotation[+S[+_], +M](self: S[Annotation[S, M]], metadata: M)
