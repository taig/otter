package io.taig.otter

import io.taig.otter as Plain

trait Syntax extends Types:
  extension [S[a] <: Plain.Schema[a], M[+_], A](self: Annotation[S[A], M])
    def imap[B](f: A => B)(g: B => A): Annotation[self.self.Self[B], M] =
      Annotation(self.self.imap(f)(g), ???)
// given toMetadata[S, M]: Conversion[Annotation[S, M], M] = _.metadata
