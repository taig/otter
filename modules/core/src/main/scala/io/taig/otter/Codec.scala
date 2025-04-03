package io.taig.otter

import cats.syntax.all.*
import cats.~>

abstract class Codec[+S[_], A] extends Product with Serializable:
  def metadata: Metadata
  def modifyMetadata(f: Metadata => Metadata): Codec[S, A]
  def imap[B](f: A => B)(g: B => A): Codec[S, B]
  def mapK[S1[a] >: S[a], T[_]](fK: S1 ~> T): Codec[T, A]
