package io.taig.otter

abstract class Codec[+M, A]:
  type Self[+m, a] <: Codec[m, a]
  type Optional[+m, a] <: Codec[m, a]

  def metadata: M

  def imap[B](f: A => B)(g: B => A): Self[M, B]
  def update[N](f: M => N): Self[N, A]
  def optional: Optional[M, Option[A]]

  def toProductWith[N](f: M => N): Product.Of[this.type, N, A] = Product.One(f(metadata), this)
