package io.taig.otter.schema

import cats.data.NonEmptyChain
import cats.syntax.all.*
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
    final def +:[B](schema: Value[B]): Self[Either[B, A]] = schema.toUnion.orElse(self.toUnion)

    final def toUnion: Self[A] = lift(self)

  extension [A <: Matchable](self: Self[A])
    inline def or[B <: Matchable](schema: Self[B]): Self[A | B] = self
      .orElse(schema)
      .imap {
        case Left(a)  => a
        case Right(b) => b
      } {
        case a: A => Left(a)
        case b: B => Right(b)
      }

    @targetName("union | value")
    inline def |[B <: Matchable](schema: Value[B]): Self[A | B] = or(schema.toUnion)

  extension [A <: Matchable](self: Value[A])
    @targetName("value | value")
    inline def |[B <: Matchable](schema: Value[B]): Self[A | B] = self.toUnion.or(schema.toUnion)

  final override def imapK[T[_]](fK: [A] => Self[A] => T[A])(gK: [A] => T[A] => Self[A]): UnionSchema[T, Value] =
    new UnionSchema[T, Value]:
      override def lift[A](schema: => Value[A]): T[A] = fK(self.lift(schema))
      override def imap[A, B](ta: T[A])(f: A => B)(g: B => A): T[B] = fK(self.imap(gK(ta))(f)(g))

      extension [A](ta: T[A])
        override def schemas: NonEmptyChain[Reference[Value, ?]] = self.schemas(gK(ta))
        override def orElse[B](schema: T[B]): T[Either[A, B]] = fK(self.orElse(gK(ta))(gK(schema)))

object UnionSchema:
  inline def apply[Self[_], Value[_]](using self: UnionSchema[Self, Value]): UnionSchema[Self, Value] = self
