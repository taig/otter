package io.taig.otter.operation

import cats.data.NonEmptyChain
import cats.syntax.all.*
import io.taig.otter.Reference

trait UnionSchemaInvariant[Self[_], Value[_]] extends SchemaInvariant[Self]:
  self =>

  def lift[A](schema: => Value[A]): Self[A]

  extension [A](self: Self[A])
    def schemas: NonEmptyChain[Reference[Value, ?]]

    def orElse[B](schema: Self[B]): Self[Either[A, B]]

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

  override def imapK[T[_]](fK: [A] => Self[A] => T[A])(gK: [A] => T[A] => Self[A]): UnionSchemaInvariant[T, Value] =
    new UnionSchemaInvariant[T, Value]:
      override def lift[A](schema: => Value[A]): T[A] = fK(self.lift(schema))
      override def enriched[A]: Enriched[T[A]] = self.enriched[A].imap(fK(_))(gK(_))
      override def imap[A, B](ta: T[A])(f: A => B)(g: B => A): T[B] = fK(self.imap(gK(ta))(f)(g))

      extension [A](ta: T[A])
        override def schemas: NonEmptyChain[Reference[Value, ?]] = self.schemas(gK(ta))
        override def orElse[B](schema: T[B]): T[Either[A, B]] = fK(self.orElse(gK(ta))(gK(schema)))

object UnionSchemaInvariant:
  inline def apply[Self[_], Value[_]](using
      self: UnionSchemaInvariant[Self, Value]
  ): UnionSchemaInvariant[Self, Value] = self
