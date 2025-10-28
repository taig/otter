package io.taig.otter.codec

trait Printer[-S[_]] extends Encoder[S, String]:
  self =>

  def print[A](schema: S[A], a: A): String

  final override inline def encode[A](schema: S[A], a: A): String = print(schema, a)

  override def contramapK[U[_]](gK: [A] => U[A] => S[A]): Encoder[U, String] = new Encoder[U, String]:
    override def encode[A](schema: U[A], a: A): String = self.encode(gK(schema), a)
