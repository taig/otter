package io.taig.otter.schema

import cats.data.NonEmptyChain
import io.taig.otter.Metadata
import io.taig.otter.Reference

import scala.annotation.targetName

trait UnionSchema[Self[_], Value[_]] extends Schema[Self]:
  self =>

  def lift[A](schema: => Value[A]): Self[A]

  extension [A](self: Self[A])
    def schemas: NonEmptyChain[Reference[Value, ?]]

    def orElse[B](schema: Self[B]): Self[Either[A, B]]

    @targetName("union :+ value")
    final def :+[B](schema: Value[B]): Self[Either[A, B]] = orElse(schema.toUnion)

  extension [A](self: Value[A])
    @targetName("value :+ value")
    final def :+[B](schema: Value[B]): Self[Either[A, B]] = self.toUnion :+ schema

    @targetName("value +: value")
    final def +:[B](schema: Value[B]): Self[Either[A, B]] = ???

    final def toUnion: Self[A] = lift(self)

  final override def imapK[T[_]](fK: [A] => Self[A] => T[A])(gK: [A] => T[A] => Self[A]): UnionSchema[T, Value] =
    new UnionSchema[T, Value]:
      override def lift[A](schema: => Value[A]): T[A] = fK(self.lift(schema))
      override def imap[A, B](ta: T[A])(f: A => B)(g: B => A): T[B] = fK(self.imap(gK(ta))(f)(g))

      extension [A](ta: T[A])
        override def metadata: Metadata = self.metadata(gK(ta))
        override def modifyMetadata(f: Metadata => Metadata): T[A] = fK(self.modifyMetadata(gK(ta))(f))
        override def schemas: NonEmptyChain[Reference[Value, ?]] = self.schemas(gK(ta))
        override def orElse[B](schema: T[B]): T[Either[A, B]] = fK(self.orElse(gK(ta))(gK(schema)))

object UnionSchema:
  inline def apply[Self[_], Value[_]](using self: UnionSchema[Self, Value]): UnionSchema[Self, Value] = self
