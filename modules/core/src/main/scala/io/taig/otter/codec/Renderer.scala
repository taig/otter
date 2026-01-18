package io.taig.otter.codec

trait Renderer[-F[_], T]:
  self =>

  def render[A](fa: F[A]): T

  def map[U](f: T => U): Renderer[F, U] = new Renderer[F, U]:
    override def render[A](fa: F[A]): U = f(self.render(fa))

  def contramapK[G[_]](fK: [A] => G[A] => F[A]): Renderer[G, T] = new Renderer[G, T]:
    override def render[A](fa: G[A]): T = self.render(fK(fa))
