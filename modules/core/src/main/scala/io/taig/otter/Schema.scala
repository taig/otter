package io.taig.otter

abstract class Schema[+A, B]:
  self =>
  type Self[+a, b] <: Schema[a, b]
  type Optional[+a, b] <: Schema[a, b]

  def metadata: A

  def update[C](f: A => C): Self[C, B]
  def imap[C](f: B => C)(g: C => B): Self[A, C]
  def optional: Optional[A, Option[B]]
