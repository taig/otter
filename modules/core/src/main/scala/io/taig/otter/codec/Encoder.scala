package io.taig.otter.codec

trait Encoder[-S[_], T]:
  self =>

  def encode[A](schema: S[A], a: A): T

  def contramapK[U[_]](gK: [A] => U[A] => S[A]): Encoder[U, T] = new Encoder[U, T]:
    def encode[A](schema: U[A], a: A): T = self.encode(gK(schema), a)
