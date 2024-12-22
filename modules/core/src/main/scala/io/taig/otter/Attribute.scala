package io.taig.otter

abstract class Attribute[+S, A]:
  def value: A

  def apply(a: A): S

object Attribute:
  abstract class Optional[+S, A] extends Attribute[S, Option[A]]:
    final inline def apply(a: A): S = apply(Some(a))
    final def clear: S = apply(None)

  object Optional:
    def apply[S: Metadata.Ops, A](self: S, key: Metadata.Key[A]): Attribute.Optional[S, A] =
      new Optional[S, A]:
        override def value: Option[A] = self.metadata.get(key)
        override def apply(a: Option[A]): S = self.attr(key, a)

  abstract class Collection[+S, A] extends Attribute[S, Vector[A]]:
    final inline def apply(as: A*): S = apply(value ++ as)
    final def clear: S = apply(Vector.empty)

  object Collection:
    def apply[S: Metadata.Ops, A](self: S, key: Metadata.Key[Vector[A]]): Attribute.Collection[S, A] =
      new Collection[S, A]:
        override def value: Vector[A] = self.metadata.get(key).getOrElse(Vector.empty)
        override def apply(a: Vector[A]): S = self.attr(key, a)

  def apply[S, A](self: S, key: Metadata.Key[A], default: => A)(using Metadata.Ops[S]): Attribute[S, A] =
    new Attribute[S, A]:
      override def value: A = self.metadata.get(key).getOrElse(default)
      override def apply(a: A): S = self.attr(key, a)
