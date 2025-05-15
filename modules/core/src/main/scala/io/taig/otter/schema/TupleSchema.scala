package io.taig.otter.schema

import io.taig.otter.Invariant
import io.taig.otter.Metadata

trait TupleSchema[Self[_], Value[_]] extends Schema[Self], Invariant.Product[Self]:
  self =>

  def empty: Self[Unit]
  def one[A](codec: => Value[A]): Self[A]

  final override def imapK[T[_]](fK: [A] => Self[A] => T[A])(gK: [A] => T[A] => Self[A]): TupleSchema[T, Value] =
    new TupleSchema[T, Value]:
      override def empty: T[Unit] = fK(self.empty)
      override def one[A](codec: => Value[A]): T[A] = fK(self.one(codec))

      extension [A](ta: T[A])
        override def metadata: Metadata = self.metadata(gK(ta))
        override def modifyMetadata(f: Metadata => Metadata): T[A] = fK(self.modifyMetadata(gK(ta))(f))
        override def imap[B](f: A => B)(g: B => A): T[B] = fK(self.imap(gK(ta))(f)(g))
        override def zip[B](schema: T[B]): T[(A, B)] = fK(self.zip(gK(ta))(gK(schema)))

object TupleSchema:
  inline def apply[Self[_], Field[_]](using self: TupleSchema[Self, Field]): TupleSchema[Self, Field] = self
