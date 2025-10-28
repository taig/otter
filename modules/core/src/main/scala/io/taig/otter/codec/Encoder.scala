package io.taig.otter.codec

trait Encoder[-S[_], T]:
  self =>

  def encode[A](schema: S[A], a: A): T

  def map[U](f: T => U): Encoder[S, U] = new Encoder[S, U]:
    override def encode[A](schema: S[A], a: A): U = f(self.encode(schema, a))

  def mapWithSchema[U](f: [A] => (S[A], T) => U): Encoder[S, U] = new Encoder[S, U]:
    override def encode[A](schema: S[A], a: A): U = f(schema, self.encode(schema, a))

  def contramapK[U[_]](gK: [A] => U[A] => S[A]): Encoder[U, T] = new Encoder[U, T]:
    override def encode[A](schema: U[A], a: A): T = self.encode(gK(schema), a)
