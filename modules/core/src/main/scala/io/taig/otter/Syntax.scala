package io.taig.otter

import io.taig.otter as Plain
import cats.syntax.all.*
import cats.Functor

trait Syntax extends Types:
  implicit def toMetadata[S, M[+_]](annotation: Annotation[S, M]): M[Annotation[S, M]] = annotation.metadata

  extension [S[a] <: Plain.Schema[a], M[+_]: Functor, A](annotation: Annotation[S[A], M])
    def imap[B](f: A => B)(g: B => A): Annotation[annotation.self.Self[B], M] = ???
