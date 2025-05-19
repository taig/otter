package io.taig.otter

final case class Enriched[S[_], A](self: S[A], metadata: Metadata):
  def mapF[T[_], B](f: S[A] => T[B]): Enriched[T, B] = copy(self = f(self))

object Enriched:
  def apply[S[_], A](self: S[A]): Enriched[S, A] = Enriched(self, metadata = Metadata.Empty)
