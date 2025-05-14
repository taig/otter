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
  trait Shape[S[_]] extends Invariant[S]:
    def imapK[T[_]](fK: [A] => S[A] => T[A])(gK: [A] => T[A] => S[A]): Shape[T]

    extension [A](self: S[A])
      def metadata: Metadata
      def modifyMetadata(f: Metadata => Metadata): S[A]

      final def metadata[B](key: Metadata.Key[B]): Option[B] = metadata.get(key)
      final def metadata[B](key: Metadata.Key[B], value: B): S[A] = modifyMetadata(_.put(key, value))
