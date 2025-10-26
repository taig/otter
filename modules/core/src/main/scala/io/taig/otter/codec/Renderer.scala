package io.taig.otter.codec

trait Renderer[-S[_], T]:
  self =>

  def render[A](schema: S[A]): T

  def contramapK[U[_]](fK: [A] => U[A] => S[A]): Renderer[U, T] = new Renderer[U, T]:
    override def render[A](schema: U[A]): T = self.render(fK(schema))
