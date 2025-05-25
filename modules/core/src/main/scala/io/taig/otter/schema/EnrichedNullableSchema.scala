package io.taig.otter.schema

import io.taig.otter.Metadata
import io.taig.otter.Enrichment

trait EnrichedNullableSchema[Self[_], Value[_]] extends NullableSchema[Self, Value], EnrichedSchema[Self]:
  self =>

  override def imapK[T[_]](fK: [A] => Self[A] => T[A])(gK: [A] => T[A] => Self[A]): EnrichedNullableSchema[T, Value] =
    new EnrichedNullableSchema[T, Value]:
      extension [A](ta: T[A])
        override def metadata: Metadata = self.metadata(gK(ta))
        override def metadata(f: Metadata => Metadata): T[A] = fK(self.metadata(gK(ta))(f))

      override def imap[A, B](fa: T[A])(f: A => B)(g: B => A): T[B] = fK(self.imap(gK(fa))(f)(g))
      override def apply[A](schema: => Value[A]): T[Option[A]] = fK(self(schema))
      override def apply[A](schema: => Value[A], default: A): T[A] = fK(self(schema, default))
      override def void: T[Unit] = fK(self.void)

object EnrichedNullableSchema:
  inline def apply[Self[_], Value[_]](using
      schema: EnrichedNullableSchema[Self, Value]
  ): EnrichedNullableSchema[Self, Value] = schema

  given [Self[_], Value[_]](using
      self: NullableSchema[Self, Value],
      enrichment: EnrichedSchema[Enrichment[Self, *]]
  ): EnrichedNullableSchema[Enrichment[Self, *], Value] =
    val nullable: NullableSchema[Enrichment[Self, *], Value] =
      self.imapK(Enrichment.liftK[Self])(Enrichment.unliftK[Self])

    new EnrichedNullableSchema[Enrichment[Self, *], Value]:
      export nullable.{apply, void}
      export enrichment.{imap, metadata}
