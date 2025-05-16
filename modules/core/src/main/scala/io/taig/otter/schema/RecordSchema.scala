package io.taig.otter.schema

import io.taig.otter.Metadata

trait RecordSchema[Self[_], Field[_]] extends Schema[Self]:
  self =>

  def lift[A](field: => Field[A]): Self[A]

  def zip[A, B](self: Self[A])(schema: Self[B]): Self[(A, B)]
  def optional[A](self: Self[A]): Self[Option[A]]

  final override def imapK[T[_]](fK: [A] => Self[A] => T[A])(gK: [A] => T[A] => Self[A]): RecordSchema[T, Field] =
    new RecordSchema[T, Field]:
      override def lift[A](field: => Field[A]): T[A] = fK(self.lift(field))
      override def zip[A, B](ta: T[A])(schema: T[B]): T[(A, B)] = fK(self.zip(gK(ta))(gK(schema)))
      override def optional[A](ta: T[A]): T[Option[A]] = fK(self.optional(gK(ta)))
      override def metadata[A](ta: T[A]): Metadata = self.metadata(gK(ta))
      override def modifyMetadata[A](ta: T[A])(f: Metadata => Metadata): T[A] = fK(self.modifyMetadata(gK(ta))(f))
      override def imap[A, B](ta: T[A])(f: A => B)(g: B => A): T[B] = fK(self.imap(gK(ta))(f)(g))

object RecordSchema:
  inline def apply[Self[_], Field[_]](using self: RecordSchema[Self, Field]): RecordSchema[Self, Field] = self
