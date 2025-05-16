package io.taig.otter.schema

import cats.Eq
import io.taig.otter.Metadata

trait ConstantSchema[Self[_], Value[_]] extends Schema[Self]:
  self =>

  def apply[A: Eq](schema: => Value[A], value: A): Self[Unit]

  final override def imapK[T[_]](fK: [A] => Self[A] => T[A])(gK: [A] => T[A] => Self[A]): ConstantSchema[T, Value] =
    new ConstantSchema[T, Value]:
      override def apply[A](schema: => Value[A], value: A)(using Eq[A]): T[Unit] = fK(self.apply(schema, value))
      override def metadata[A](ta: T[A]): Metadata = self.metadata(gK(ta))
      override def modifyMetadata[A](ta: T[A])(f: Metadata => Metadata): T[A] = fK(self.modifyMetadata(gK(ta))(f))
      override def imap[A, B](ta: T[A])(f: A => B)(g: B => A): T[B] = fK(self.imap(gK(ta))(f)(g))

object ConstantSchema:
  inline def apply[Self[_], Value[_]](using self: ConstantSchema[Self, Value]): ConstantSchema[Self, Value] = self
