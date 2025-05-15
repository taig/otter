package io.taig.otter.schema

import io.taig.otter.Metadata
import io.taig.otter.Reference
import io.taig.otter.Reference.Constant

trait FieldSchema[Self[_], Key[_], Value[_]] extends Schema[Self]:
  self =>

  def field[A, B](name: A, key: => Key[A], value: => Value[B]): Self[B]

  extension [A](self: Self[A])
    def key: Reference.Constant[Key, ?]
    def value: Reference[Value, ?]

  final override def imapK[T[_]](fK: [A] => Self[A] => T[A])(gK: [A] => T[A] => Self[A]): FieldSchema[T, Key, Value] =
    new FieldSchema[T, Key, Value]:
      override def field[A, B](name: A, key: => Key[A], value: => Value[B]): T[B] = ???

      extension [A](ta: T[A])
        override def metadata: Metadata = self.metadata(gK(ta))
        override def modifyMetadata(f: Metadata => Metadata): T[A] = fK(self.modifyMetadata(gK(ta))(f))
        override def key: Constant[Key, ?] = self.key(gK(ta))
        override def value: Reference[Value, ?] = self.value(gK(ta))
        override def imap[B](f: A => B)(g: B => A): T[B] = fK(self.imap(gK(ta))(f)(g))
