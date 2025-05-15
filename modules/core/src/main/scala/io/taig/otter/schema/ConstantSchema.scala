package io.taig.otter.schema

import io.taig.otter.Metadata
import cats.Eq

trait ConstantSchema[Self[_], Value[_]] extends Schema[Self]:
  self =>

  final override def imapK[T[_]](fK: [A] => Self[A] => T[A])(gK: [A] => T[A] => Self[A]): ConstantSchema[T, Value] =
    new ConstantSchema[T, Value]:
      override def constant[A](schema: => Value[A], value: A)(using Eq[A]): T[Unit] = fK(self.constant(schema, value))

      extension [A](fa: T[A])
        override def metadata: Metadata = self.metadata(gK(fa))
        override def modifyMetadata(f: Metadata => Metadata): T[A] = fK(self.modifyMetadata(gK(fa))(f))
        override def imap[B](f: A => B)(g: B => A): T[B] = fK(self.imap(gK(fa))(f)(g))

  def constant[A: Eq](schema: => Value[A], value: A): Self[Unit]

object ConstantSchema:
  inline def apply[Self[_], Value[_]](using self: ConstantSchema[Self, Value]): ConstantSchema[Self, Value] = self
