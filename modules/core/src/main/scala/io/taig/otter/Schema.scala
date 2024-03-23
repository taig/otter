package io.taig.otter

abstract class Schema[+M, A]:
  type Self[+m, a] <: Schema[m, a]
  type Optional[+m, a] <: Schema[m, a]

  def metadata: M

  def imap[B](f: A => B)(g: B => A): Self[M, B]
  def update[N](f: M => N): Self[N, A]
  def optional: Optional[M, Option[A]]

  def toProductWith[N](f: M => N): Product.Of[this.type, N, A] = Product.One(f(metadata), this)
