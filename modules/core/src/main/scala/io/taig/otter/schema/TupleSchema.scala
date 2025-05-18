package io.taig.otter.schema

import io.taig.otter.Metadata
import io.taig.otter.Merge
import io.taig.otter.syntax.InvariantSyntax.*

trait TupleSchema[Self[_], Value[_]] extends Schema[Self]:
  self =>

  def empty: Self[Unit]
  def lift[A](schema: => Value[A]): Self[A]

  extension [A](self: Self[A])
    def zip[B](schema: Self[B]): Self[(A, B)]
    def :*[B](schema: Value[B])(using merge: Merge[A, B]): Self[merge.Out] =
      ???

  final override def imapK[T[_]](fK: [A] => Self[A] => T[A])(gK: [A] => T[A] => Self[A]): TupleSchema[T, Value] =
    new TupleSchema[T, Value]:
      override def empty: T[Unit] = fK(self.empty)
      override def lift[A](schema: => Value[A]): T[A] = fK(self.lift(schema))
      override def imap[A, B](ta: T[A])(f: A => B)(g: B => A): T[B] = fK(self.imap(gK(ta))(f)(g))

      extension [A](ta: T[A])
        override def metadata: Metadata = self.metadata(gK(ta))
        override def modifyMetadata(f: Metadata => Metadata): T[A] = fK(self.modifyMetadata(gK(ta))(f))
        override def zip[B](schema: T[B]): T[(A, B)] = fK(self.zip(gK(ta))(gK(schema)))

object TupleSchema:
  inline def apply[Self[_], Field[_]](using self: TupleSchema[Self, Field]): TupleSchema[Self, Field] = self
