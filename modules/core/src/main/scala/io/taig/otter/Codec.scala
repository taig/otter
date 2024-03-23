package io.taig.otter

abstract class Codec[+M, A]:
  self =>
  type Metadata <: Codec.Metadata
  type Self[+m, a] <: Codec[m, a]
  type Optional[+m, a] <: Codec[m, a]

  def metadata: M & Metadata

  def imap[B](f: A => B)(g: B => A): Self[M, B]
  def update[N <: Metadata](f: M => N): Self[N, A]
  def optional: Optional[M, Option[A]]

object Codec:
  trait Metadata:
    type Self <: Codec.Metadata
