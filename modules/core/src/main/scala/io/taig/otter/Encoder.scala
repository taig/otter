package io.taig.otter

trait Encoder[A]:
  self =>
  def encode(a: A): OpenApi

  final def contramap[B](f: B => A): Encoder[B] = new Encoder[B]:
    override def encode(b: B): OpenApi = self.encode(f(b))

object Encoder:
  inline def apply[A](using encoder: Encoder[A]): Encoder[A] = encoder

  given Encoder[Long] with
    override def encode(a: Long): OpenApi = OpenApi.Integer(a)
