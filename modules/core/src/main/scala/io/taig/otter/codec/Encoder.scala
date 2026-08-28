package io.taig.otter.codec

/** Writes a value described by the schema `F`. Only the write direction of the schema is consumed, so a schema whose
  * write side is [[io.taig.otter.Void]] cannot be passed here.
  */
trait Encoder[-F[-_, +_], T]:
  self =>

  def encode[W](fa: F[W, Any], w: W): T

  def map[U](f: T => U): Encoder[F, U] = new Encoder[F, U]:
    override def encode[W](fa: F[W, Any], w: W): U = f(self.encode(fa, w))

  def contramapK[G[-_, +_]](fK: [w, r] => G[w, r] => F[w, r]): Encoder[G, T] = new Encoder[G, T]:
    override def encode[W](ga: G[W, Any], w: W): T = self.encode(fK(ga), w)

object Encoder:
  def apply[F[-_, +_], T](f: [w] => (F[w, Any], w) => T): Encoder[F, T] = new Encoder[F, T]:
    override def encode[W](fa: F[W, Any], w: W): T = f(fa, w)
