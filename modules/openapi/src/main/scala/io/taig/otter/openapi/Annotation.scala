package io.taig.otter.openapi

import cats.Functor
import cats.syntax.all.*
import io.taig.otter.Fix

final case class Annotation[+S[+_], +M](self: S[Annotation[S, M]], metadata: M)

object Annotation:
  extension [S[+_], M](self: Annotation[S, M]) def toFix(using Functor[S]): Fix[S] = Fix(self.self.map(_.toFix))
