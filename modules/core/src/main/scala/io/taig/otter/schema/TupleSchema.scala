package io.taig.otter.schema

import io.taig.otter.Metadata

trait TupleSchema[Self[_], Value[_]] extends Schema[Self]:
  self =>

  def empty: Self[Unit]
  def lift[A](schema: => Value[A]): Self[A]

  def zip[A, B](self: Self[A])(schema: Self[B]): Self[(A, B)]

  final override def imapK[T[_]](fK: [A] => Self[A] => T[A])(gK: [A] => T[A] => Self[A]): TupleSchema[T, Value] =
    new TupleSchema[T, Value]:

      override def empty: T[Unit] = fK(self.empty)
      override def lift[A](schema: => Value[A]): T[A] = fK(self.lift(schema))

      override def zip[A, B](ta: T[A])(schema: T[B]): T[(A, B)] = fK(self.zip(gK(ta))(gK(schema)))
      override def metadata[A](ta: T[A]): Metadata = self.metadata(gK(ta))
      override def modifyMetadata[A](ta: T[A])(f: Metadata => Metadata): T[A] = fK(self.modifyMetadata(gK(ta))(f))
      override def imap[A, B](ta: T[A])(f: A => B)(g: B => A): T[B] = fK(self.imap(gK(ta))(f)(g))

object TupleSchema:
  inline def apply[Self[_], Field[_]](using self: TupleSchema[Self, Field]): TupleSchema[Self, Field] = self
