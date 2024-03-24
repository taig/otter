package io.taig.otter

abstract class Schema[M, A]:
  type Self[a] <: Schema[M, a]
  type Optional[a] <: Schema[M, a]

  def metadata: M

  def imap[B](f: A => B)(g: B => A): Self[B]
  def optional: Optional[Option[A]]

  def toProductWith[N](f: M => N): Product.Of[this.type, N, A] = Product.One(f(metadata), this)
