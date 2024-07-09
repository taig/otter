package io.taig.otter

trait Primitive[A] extends Value[Nothing, Nothing, A], Primitive.Reader[A], Primitive.Writer[A]

object Primitive:
  trait Reader[+A] extends Value.Reader[Nothing, Nothing, A]

  trait Writer[-A] extends Value.Writer[Nothing, Nothing, A]

  def apply[A](tpe: Type[A]): Primitive[A] = ???
