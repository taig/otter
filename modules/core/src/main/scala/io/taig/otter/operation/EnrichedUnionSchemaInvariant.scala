package io.taig.otter.operation

import io.taig.otter.Reference

import cats.data.NonEmptyChain

import io.taig.otter.Metadata
import io.taig.otter.Enrichment

trait EnrichedUnionSchemaInvariant[Self[_], Value[_]]
    extends UnionSchemaInvariant[Self, Value],
      EnrichedSchemaInvariant[Self]:
  self =>

  override def imapK[T[_]](fK: [A] => Self[A] => T[A])(
      gK: [A] => T[A] => Self[A]
  ): EnrichedUnionSchemaInvariant[T, Value] =
    new EnrichedUnionSchemaInvariant[T, Value]:
      override def lift[A](schema: => Value[A]): T[A] = ???

      extension [A](ta: T[A])
        override def schemas: NonEmptyChain[Reference[Value, ?]] = self.schemas(gK(ta))
        override def orElse[B](schema: T[B]): T[Either[A, B]] = fK(self.orElse(gK(ta))(gK(schema)))

        override def metadata: Metadata = self.metadata(gK(ta))
        override def metadata(f: Metadata => Metadata): T[A] = fK(self.metadata(gK(ta))(f))

      override def imap[A, B](fa: T[A])(f: A => B)(g: B => A): T[B] = fK(self.imap(gK(fa))(f)(g))

object EnrichedUnionSchemaInvariant:
  inline def apply[Self[_], Value[_]](using
      schema: EnrichedUnionSchemaInvariant[Self, Value]
  ): EnrichedUnionSchemaInvariant[Self, Value] = schema

  given [Self[_], Value[_]](using
      self: UnionSchemaInvariant[Self, Value],
      enrichment: EnrichedSchemaInvariant[Enrichment[Self, *]]
  ): EnrichedUnionSchemaInvariant[Enrichment[Self, *], Value] =
    val union: UnionSchemaInvariant[Enrichment[Self, *], Value] =
      self.imapK(Enrichment.liftK[Self])(Enrichment.unliftK[Self])

    new EnrichedUnionSchemaInvariant[Enrichment[Self, *], Value]:
      export union.{lift, orElse, schemas}
      export enrichment.{imap, metadata}
