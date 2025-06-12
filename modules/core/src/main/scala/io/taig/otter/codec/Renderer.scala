package io.taig.otter.codec

import cats.~>

trait Renderer[S[_], T]:
  self =>

  def render[A](schema: S[A]): T

  final def map[B](f: T => B): Renderer[S, B] = new Renderer[S, B]:
    override def render[A](schema: S[A]): B = f(self.render(schema))

  def mapK[U[_]](fK: U ~> S): Renderer[U, T] = new Renderer[U, T]:
    override def render[A](schema: U[A]): T = self.render(schema = fK(schema))
