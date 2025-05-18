package io.taig.otter.schema

import io.taig.otter.Metadata

trait SumSchema[Self[_], Branch[_]] extends Schema[Self]:
  self =>

  def lift[A](branch: => Branch[A]): Self[A]

  extension [A](self: Self[A])
    def orElse[B](schema: Self[B]): Self[Either[A, B]]

  final override def imapK[T[_]](fK: [A] => Self[A] => T[A])(gK: [A] => T[A] => Self[A]): SumSchema[T, Branch] =
    new SumSchema[T, Branch]:
      override def lift[A](branch: => Branch[A]): T[A] = fK(self.lift(branch))
      override def imap[A, B](ta: T[A])(f: A => B)(g: B => A): T[B] = fK(self.imap(gK(ta))(f)(g))

      extension [A](ta: T[A])
        override def metadata: Metadata = self.metadata(gK(ta))
        override def modifyMetadata(f: Metadata => Metadata): T[A] = fK(self.modifyMetadata(gK(ta))(f))
        override def orElse[B](schema: T[B]): T[Either[A, B]] = fK(self.orElse(gK(ta))(gK(schema)))

object SumSchema:
  inline def apply[Self[_], Branch[_]](using self: SumSchema[Self, Branch]): SumSchema[Self, Branch] = self
