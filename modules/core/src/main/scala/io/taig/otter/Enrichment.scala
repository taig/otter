package io.taig.otter

import cats.syntax.all.*

final case class Enrichment[+S[_], A](self: S[A], metadata: Metadata):
  def mapF[T[_], B](f: S[A] => T[B]): Enrichment[T, B] = copy(self = f(self))

  def modifyMetadata(f: Metadata => Metadata): Enrichment[S, A] = copy(metadata = f(metadata))

object Enrichment:
  def apply[S[_], A](self: S[A]): Enrichment[S, A] = Enrichment(self, metadata = Metadata.Empty)
