package io.taig.otter.codec

trait Encoder[-F[_], T]:
  self =>

  def encode[A](fa: F[A], a: A): T

  def map[U](f: T => U): Encoder[F, U] = new Encoder[F, U]:
    override def encode[A](fa: F[A], a: A): U = f(self.encode(fa, a))

  def mapWith[F1[a] <: F[a], U](f: [A] => (F1[A], T) => U): Encoder[F1, U] = new Encoder[F1, U]:
    override def encode[A](fa: F1[A], a: A): U = f(fa, self.encode(fa, a))

  def mapK[F1[a] <: F[a], G[_]](fK: [A] => F1[A] => G[A]): Encoder[G, T] = ???

  def contramapK[G[_]](fK: [A] => G[A] => F[A]): Encoder[G, T] = new Encoder[G, T]:
    override def encode[A](ga: G[A], a: A): T = self.encode(fK(ga), a)
