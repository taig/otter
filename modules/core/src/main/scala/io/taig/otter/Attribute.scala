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
        override def value: Option[A] = self.apply(key)
        override def apply(a: Option[A]): S = self.apply(key, a)

  def apply[S: Metadata.Ops, A](self: S, key: Metadata.Key[A], default: => A): Attribute[S, A] =
    new Attribute[S, A]:
      override def value: A = self.apply(key).getOrElse(default)
      override def apply(a: A): S = self.apply(key, a)
