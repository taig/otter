package io.taig.otter.schema

import io.taig.otter.Metadata

trait NullableSchema[Self[_], Value[_]] extends Schema[Self]:
  self =>

  def nullable[A](schema: => Value[A]): Self[Option[A]]
  def nullable[A](schema: => Value[A], default: A): Self[A]
  def void: Self[Unit]

  override def imapK[T[_]](fK: [A] => Self[A] => T[A])(gK: [A] => T[A] => Self[A]): NullableSchema[T, Value] =
    new NullableSchema[T, Value]:
      override def nullable[A](schema: => Value[A]): T[Option[A]] = fK(self.nullable(schema))
      override def nullable[A](schema: => Value[A], default: A): T[A] = fK(self.nullable(schema, default))
      override def void: T[Unit] = fK(self.void)

      extension [A](ta: T[A])
        override def imap[B](f: A => B)(g: B => A): T[B] = fK(self.imap(gK(ta))(f)(g))
        override def metadata: Metadata = self.metadata(gK(ta))
        override def modifyMetadata(f: Metadata => Metadata): T[A] = fK(self.modifyMetadata(gK(ta))(f))

object NullableSchema:
  inline def apply[Self[_], Value[_]](using self: NullableSchema[Self, Value]): NullableSchema[Self, Value] = self
