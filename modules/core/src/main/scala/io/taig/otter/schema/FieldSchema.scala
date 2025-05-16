package io.taig.otter.schema

import io.taig.otter.Metadata
import io.taig.otter.Reference
import io.taig.otter.Reference.Constant
import io.taig.otter.Merge

trait FieldSchema[Self[_], Key[_], Value[_]] extends Schema[Self]:
  self =>

  def apply[A, B](name: A, key: => Key[A], value: => Value[B]): Self[B]

  def key[A](self: Self[A]): Reference.Constant[Key, ?]
  def value[A](self: Self[A]): Reference[Value, ?]

  def nullish[A](self: Self[A]): Boolean
  def modifyNullish[A](self: Self[A])(f: Boolean => Boolean): Self[A]

  def optional[A](self: Self[A]): Self[Option[A]]

  // final override def imapK[T[_]](fK: [A] => Self[A] => T[A])(
  //     gK: [A] => T[A] => Self[A]
  // ): FieldSchema[T, Key, Value] = new FieldSchema[T, Key, Value]:
  //   override def apply[A, B](name: A, key: => Key[A], value: => Value[B]): T[B] =
  //     fK(self.apply(name, key, value))

  //   extension [A](ta: T[A])
  //     override def key: Constant[Key, ?] = self.key(gK(ta))
  //     override def value: Reference[Value, ?] = self.value(gK(ta))
  //     override def metadata: Metadata = self.metadata(gK(ta))
  //     override def modifyMetadata(f: Metadata => Metadata): T[A] = fK(self.modifyMetadata(gK(ta))(f))
  //     override def imap[B](f: A => B)(g: B => A): T[B] = fK(self.imap(gK(ta))(f)(g))
  //     override def optional: T[Option[A]] = fK(self.optional(gK(ta)))
  //     override def nullish: T[A] = fK(self.nullish(gK(ta)))
