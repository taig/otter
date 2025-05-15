package io.taig.otter.schema

import io.taig.otter.Metadata

trait UnionSchema[Self[_], Value[_]] extends Schema[Self]:
  self =>

  def lift[A](codec: => Value[A]): Self[A]

  extension [A](self: Self[A]) def orElse[B](schema: Self[B]): Self[Either[A, B]]

  final override def imapK[T[_]](fK: [A] => Self[A] => T[A])(gK: [A] => T[A] => Self[A]): UnionSchema[T, Value] =
    new UnionSchema[T, Value]:
      override def lift[A](codec: => Value[A]): T[A] = fK(self.lift(codec))

      extension [A](ta: T[A])
        override def metadata: Metadata = self.metadata(gK(ta))
        override def modifyMetadata(f: Metadata => Metadata): T[A] = fK(self.modifyMetadata(gK(ta))(f))
        override def imap[B](f: A => B)(g: B => A): T[B] = fK(self.imap(gK(ta))(f)(g))
        override def orElse[B](tb: T[B]): T[Either[A, B]] = fK(self.orElse(gK(ta))(gK(tb)))

object UnionSchema:
  inline def apply[Self[_], Value[_]](using self: UnionSchema[Self, Value]): UnionSchema[Self, Value] = self
