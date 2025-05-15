package io.taig.otter.schema

import io.taig.otter.Field
import io.taig.otter.Metadata
import io.taig.otter.Invariant

trait RecordSchema[Self[_], Field[_]] extends Schema[Self], Invariant.Product[Self]:
  self =>

  def record[A](field: => Field[A]): Self[A]

  extension [A](self: Self[A]) def optional: Self[Option[A]]

  final override def imapK[T[_]](fK: [A] => Self[A] => T[A])(gK: [A] => T[A] => Self[A]): RecordSchema[T, Field] =
    new RecordSchema[T, Field]:
      override def record[A](field: => Field[A]): T[A] = fK(self.record(field))

      extension [A](fa: T[A])
        override def metadata: Metadata = self.metadata(gK(fa))
        override def modifyMetadata(f: Metadata => Metadata): T[A] = fK(self.modifyMetadata(gK(fa))(f))
        override def imap[B](f: A => B)(g: B => A): T[B] = fK(self.imap(gK(fa))(f)(g))
        override def zip[B](schema: T[B]): T[(A, B)] = fK(self.zip(gK(fa))(gK(schema)))
        override def optional: T[Option[A]] = fK(self.optional(gK(fa)))

object RecordSchema:
  inline def apply[Self[_], Field[_]](using self: RecordSchema[Self, Field]): RecordSchema[Self, Field] = self
