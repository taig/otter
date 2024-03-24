package io.taig.otter

abstract class Schema[A]:
  type Self[a] <: Schema[a]
  type Optional[a] <: Schema[a]

  def imap[B](f: A => B)(g: B => A): Self[B]
  def optional: Optional[Option[A]]

  def toProduct: Product.Of[this.type, A] = Product.One(this)

object Schema:
  final case class With[S[a] <: Schema[a], A, B](self: S[A], value: B)
