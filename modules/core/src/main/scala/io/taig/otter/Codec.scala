package io.taig.otter

import cats.syntax.all.*

abstract class Codec[+S[_], A] extends Product with Serializable:
  def metadata: Metadata
  def modifyMetadata(f: Metadata => Metadata): Codec[S, A]
  def imap[B](f: A => B)(g: B => A): Codec[S, B]
