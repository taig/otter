package io.taig.otter.schema

import io.taig.otter.Metadata

trait SumSchema[Self[_], Branch[_]] extends Schema[Self]:
  self =>

  def lift[A](branch: Branch[A]): Self[A]

  def orElse[A, B](self: Self[A])(schema: Self[B]): Self[Either[A, B]]

  final override def imapK[T[_]](fK: [A] => Self[A] => T[A])(gK: [A] => T[A] => Self[A]): SumSchema[T, Branch] =
    new SumSchema[T, Branch]:
      override def lift[A](branch: Branch[A]): T[A] = fK(self.lift(branch))
      override def orElse[A, B](ta: T[A])(schema: T[B]): T[Either[A, B]] = fK(self.orElse(gK(ta))(gK(schema)))
      override def metadata[A](ta: T[A]): Metadata = self.metadata(gK(ta))
      override def modifyMetadata[A](ta: T[A])(f: Metadata => Metadata): T[A] = fK(self.modifyMetadata(gK(ta))(f))
      override def imap[A, B](ta: T[A])(f: A => B)(g: B => A): T[B] = fK(self.imap(gK(ta))(f)(g))

object SumSchema:
  inline def apply[Self[_], Branch[_]](using self: SumSchema[Self, Branch]): SumSchema[Self, Branch] = self
