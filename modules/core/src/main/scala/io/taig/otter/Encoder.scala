package io.taig.otter

import io.taig.otter.syntax.*

trait Encoder[-A]:
  self =>
  def encode(a: A): OpenApi

  final def contramap[B](f: B => A): Encoder[B] = new Encoder[B]:
    override def encode(b: B): OpenApi = self.encode(f(b))

object Encoder:
  inline def apply[A](using encoder: Encoder[A]): Encoder[A] = encoder

  given Encoder[OpenApi] with
    override def encode(a: OpenApi): OpenApi = a

  given Encoder[Boolean] with
    override def encode(a: Boolean): OpenApi = OpenApi.Bool(a)

  given Encoder[Long] with
    override def encode(a: Long): OpenApi = OpenApi.Integer(a)

  given Encoder[Int] with
    override def encode(a: Int): OpenApi = OpenApi.Integer(a)

  given Encoder[BigInt] with
    override def encode(a: BigInt): OpenApi = OpenApi.Integer(a)

  given Encoder[String] with
    override def encode(a: String): OpenApi = OpenApi.Text(a)

  given [A: Encoder]: Encoder[Option[A]] with
    override def encode(a: Option[A]): OpenApi = a.fold(OpenApi.Null)(_.asOpenApi)
