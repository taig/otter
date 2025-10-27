package io.taig.otter.codec

trait Renderer[-S[_], T]:
  self =>

  def render[A](schema: S[A]): T

  def map[U](f: T => U): Renderer[S, U] = new Renderer[S, U]:
    override def render[A](schema: S[A]): U = f(self.render(schema))

  def contramapK[U[_]](fK: [A] => U[A] => S[A]): Renderer[U, T] = new Renderer[U, T]:
    override def render[A](schema: U[A]): T = self.render(fK(schema))
