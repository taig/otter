package io.taig.otter.schema

import cats.~>
import io.taig.otter.Metadata
import io.taig.otter.Invariant

abstract class Schema[+S[_], A]:
  def metadata: Metadata
  def modifyMetadata(f: Metadata => Metadata): Schema[S, A]
  def imap[B](f: A => B)(g: B => A): Schema[S, B]
  def mapK[S1[a] >: S[a], T[_]](fK: S1 ~> T): Schema[T, A]

object Schema:
  trait Shape[Self[_]] extends Invariant[Self]:
    extension [A](self: Self[A])
      def metadata: Metadata
      def modifyMetadata(f: Metadata => Metadata): Self[A]

      final def metadata[B](key: Metadata.Key[B]): Option[B] = metadata.get(key)
      final def metadata[B](key: Metadata.Key[B], value: B): Self[A] = modifyMetadata(_.put(key, value))
