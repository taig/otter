package io.taig.otter

abstract class Schema[+A]:
  def imap[A1 >: A, B](f: A => B)(g: B => A1): Schema[B]
  def optional: Schema[Option[A]]

  def toProduct: Product.Of[this.type, A] = Product.One(this)

object Schema:
  final case class With[S[a] <: Schema[a], A, B](self: S[A], value: B)
