package io.taig.otter.codec

/** Writes a value described by the schema `F`.
  *
  * The schema is demanded as `F[W, Any]`, so any schema that writes `W` is accepted and a read only schema, whose write
  * side is `Nothing`, admits no value to write.
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
