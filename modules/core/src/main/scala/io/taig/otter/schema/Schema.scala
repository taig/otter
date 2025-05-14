package io.taig.otter.schema

import cats.~>
import io.taig.otter.Metadata
import io.taig.otter.Invariant
import io.taig.otter.Shape

abstract class Schema[+S[_], A]:
  def metadata: Metadata
  def modifyMetadata(f: Metadata => Metadata): Schema[S, A]
  def imap[B](f: A => B)(g: B => A): Schema[S, B]
  def mapK[S1[a] >: S[a], T[_]](fK: S1 ~> T): Schema[T, A]

object Schema:
  given [Value[_]]: Shape[Schema[Value, *]] with
    extension [A](self: Schema[Value, A])
      override def metadata: Metadata = self.metadata
      override def modifyMetadata(f: Metadata => Metadata): Schema[Value, A] = self.modifyMetadata(f)
      override def imap[B](f: A => B)(g: B => A): Schema[Value, B] = self.imap(f)(g)
