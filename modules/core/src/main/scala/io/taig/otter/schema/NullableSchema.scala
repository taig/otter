package io.taig.otter.schema

import io.taig.otter.Metadata

trait NullableSchema[Self[_], Value[_]] extends Schema[Self]:
  self =>

  def apply[A](schema: => Value[A]): Self[Option[A]]
  def apply[A](schema: => Value[A], default: A): Self[A]
  def void: Self[Unit]

  extension[A](value: Value[A])
    final def nullable: Self[Option[A]] = apply(value)
    final def nullable(default: A): Self[A] = apply(value, default)

  override def imapK[T[_]](fK: [A] => Self[A] => T[A])(gK: [A] => T[A] => Self[A]): NullableSchema[T, Value] =
    new NullableSchema[T, Value]:
      override def apply[A](schema: => Value[A]): T[Option[A]] = fK(self.apply(schema))
      override def apply[A](schema: => Value[A], default: A): T[A] = fK(self.apply(schema, default))
      override def void: T[Unit] = fK(self.void)
      override def imap[A, B](ta: T[A])(f: A => B)(g: B => A): T[B] = fK(self.imap(gK(ta))(f)(g))

      extension [A](ta: T[A])
        override def metadata: Metadata = self.metadata(gK(ta))
        override def modifyMetadata(f: Metadata => Metadata): T[A] = fK(self.modifyMetadata(gK(ta))(f))

object NullableSchema:
  inline def apply[Self[_], Value[_]](using self: NullableSchema[Self, Value]): NullableSchema[Self, Value] = self
