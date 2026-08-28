package io.taig.otter.codec

/** Turns a schema into an output without involving a value, which is what code generation needs. */
trait Renderer[-F[-_, +_], T]:
  self =>

  def render[W, R](fa: F[W, R]): T

  def map[U](f: T => U): Renderer[F, U] = new Renderer[F, U]:
    override def render[W, R](fa: F[W, R]): U = f(self.render(fa))

  def contramapK[G[-_, +_]](fK: [w, r] => G[w, r] => F[w, r]): Renderer[G, T] = new Renderer[G, T]:
    override def render[W, R](ga: G[W, R]): T = self.render(fK(ga))

object Renderer:
  def apply[F[-_, +_], T](f: [w, r] => F[w, r] => T): Renderer[F, T] = new Renderer[F, T]:
    override def render[W, R](fa: F[W, R]): T = f(fa)

  def pure[F[-_, +_], T](value: => T): Renderer[F, T] = Renderer([w, r] => (_: F[w, r]) => value)
