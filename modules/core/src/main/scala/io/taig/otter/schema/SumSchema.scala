package io.taig.otter.schema

import io.taig.otter.Metadata
import io.taig.otter.Invariant
import io.taig.otter.Branch

trait SumSchema[Self[_], Key[_], Value[_]] extends Schema[Self], Invariant.Coproduct[Self]:
  self =>

  def sum[A](branch: Branch[Key, Value, A]): Self[A]

  final override def imapK[T[_]](fK: [A] => Self[A] => T[A])(gK: [A] => T[A] => Self[A]): SumSchema[T, Key, Value] =
    new SumSchema[T, Key, Value]:
      override def sum[A](branch: Branch[Key, Value, A]): T[A] = fK(self.sum(branch))

      extension [A](ta: T[A])
        override def metadata: Metadata = self.metadata(gK(ta))
        override def modifyMetadata(f: Metadata => Metadata): T[A] = fK(self.modifyMetadata(gK(ta))(f))
        override def orElse[B](schema: T[B]): T[Either[A, B]] = fK(self.orElse(gK(ta))(gK(schema)))
        override def imap[B](f: A => B)(g: B => A): T[B] = fK(self.imap(gK(ta))(f)(g))

object SumSchema:
  inline def apply[Self[_], Key[_], Value[_]](using self: SumSchema[Self, Key, Value]): SumSchema[Self, Key, Value] =
    self
