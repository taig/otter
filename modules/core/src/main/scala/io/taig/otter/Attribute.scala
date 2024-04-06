package io.taig.otter

abstract class Attribute[S, A]:
  def value: A
  def apply(f: A => A): S
  final def apply(a: A): S = apply(_ => a)

object Attribute:
  extension [S, A](self: Attribute[S, Option[A]])
    final def apply(a: A): S = self.apply(Some(a))
    final def clear: S = self.apply(None)

  def apply[S, A](a: A)(f: (A => A) => S): Attribute[S, A] = new Attribute[S, A]:
    override def value: A = a
    override def apply(g: A => A): S = f(g)
