package io.taig.crock.schema

abstract class StarOps[F[_], G[_]](self: F[Unit]) {
  def :*[B](other: F[B]): G[B]
  def *:[B](other: F[B]): G[B]
}

abstract class StarOps2[F[_], G[_], A](self: F[A]) {
  def :*[B](other: F[B]): G[(A, B)]
  def *:[B](other: F[B]): G[(B, A)]
}

abstract class ToStarOps extends ToStarOps1 {}

abstract class ToStarOps1 extends ToStarOps2

abstract class ToStarOps2
