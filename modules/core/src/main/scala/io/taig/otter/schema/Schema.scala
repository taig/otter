package io.taig.otter.schema

import cats.Invariant
import io.taig.otter.Metadata

trait Schema[Self[_]] extends Invariant[Self]:
  self =>

  def metadata[A](self: Self[A]): Metadata
  def modifyMetadata[A](self: Self[A])(f: Metadata => Metadata): Self[A]

  def imapK[T[_]](fK: [A] => Self[A] => T[A])(gK: [A] => T[A] => Self[A]): Schema[T] = new Schema[T]:
    override def metadata[A](ta: T[A]): Metadata = self.metadata(gK(ta))
    override def modifyMetadata[A](ta: T[A])(f: Metadata => Metadata): T[A] = fK(self.modifyMetadata(gK(ta))(f))
    override def imap[A, B](ta: T[A])(f: A => B)(g: B => A): T[B] = fK(self.imap(gK(ta))(f)(g))
