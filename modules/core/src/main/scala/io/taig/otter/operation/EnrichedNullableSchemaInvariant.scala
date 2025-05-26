package io.taig.otter.operation

import io.taig.otter.Metadata
import io.taig.otter.Enrichment

trait EnrichedNullableSchemaInvariant[Self[_], Value[_]]
    extends NullableSchemaInvariant[Self, Value],
      EnrichedSchemaInvariant[Self]:
  self =>

  override def imapK[T[_]](fK: [A] => Self[A] => T[A])(
      gK: [A] => T[A] => Self[A]
  ): EnrichedNullableSchemaInvariant[T, Value] =
    new EnrichedNullableSchemaInvariant[T, Value]:
      extension [A](ta: T[A])
        override def metadata: Metadata = self.metadata(gK(ta))
        override def metadata(f: Metadata => Metadata): T[A] = fK(self.metadata(gK(ta))(f))

      override def imap[A, B](fa: T[A])(f: A => B)(g: B => A): T[B] = fK(self.imap(gK(fa))(f)(g))
      override def apply[A](schema: => Value[A]): T[Option[A]] = fK(self(schema))
      override def apply[A](schema: => Value[A], default: A): T[A] = fK(self(schema, default))
      override def void: T[Unit] = fK(self.void)

object EnrichedNullableSchemaInvariant:
  inline def apply[Self[_], Value[_]](using
      schema: EnrichedNullableSchemaInvariant[Self, Value]
  ): EnrichedNullableSchemaInvariant[Self, Value] = schema

  given [Self[_], Value[_]](using
      self: NullableSchemaInvariant[Self, Value],
      enrichment: EnrichedSchemaInvariant[Enrichment[Self, *]]
  ): EnrichedNullableSchemaInvariant[Enrichment[Self, *], Value] =
    val nullable: NullableSchemaInvariant[Enrichment[Self, *], Value] =
      self.imapK(Enrichment.liftK[Self])(Enrichment.unliftK[Self])

    new EnrichedNullableSchemaInvariant[Enrichment[Self, *], Value]:
      export nullable.{apply, void}
      export enrichment.{imap, metadata}
