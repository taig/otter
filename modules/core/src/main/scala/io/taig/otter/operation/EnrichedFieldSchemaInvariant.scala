package io.taig.otter.operation

import io.taig.otter.Reference

import io.taig.otter.Reference.Constant
import io.taig.otter.Enrichment
import io.taig.otter.Metadata

trait EnrichedFieldSchemaInvariant[Self[_], Key[_], Value[_]]
    extends FieldSchemaInvariant[Self, Key, Value],
      EnrichedSchemaInvariant[Self]:
  self =>

  override def imapK[T[_]](fK: [A] => Self[A] => T[A])(
      gK: [A] => T[A] => Self[A]
  ): EnrichedFieldSchemaInvariant[T, Key, Value] =
    new EnrichedFieldSchemaInvariant[T, Key, Value]:
      override def apply[A, B](name: A, key: => Key[A], value: => Value[B]): T[B] = fK(self(name, key, value))

      extension [A](ta: T[A])
        override def key: Constant[Key, ?] = self.key(gK(ta))
        override def value: Reference[Value, ?] = self.value(gK(ta))
        override def isOptional: Boolean = self.isOptional(gK(ta))
        override def nullish: Boolean = self.nullish(gK(ta))
        override def nullish(f: Boolean => Boolean): T[A] = fK(self.nullish(gK(ta))(f))
        override def optional: T[Option[A]] = fK(self.optional(gK(ta)))
        override def metadata: Metadata = self.metadata(gK(ta))
        override def metadata(f: Metadata => Metadata): T[A] = fK(self.metadata(gK(ta))(f))

      override def imap[A, B](fa: T[A])(f: A => B)(g: B => A): T[B] = fK(self.imap(gK(fa))(f)(g))

object EnrichedFieldSchemaInvariant:
  inline def apply[Self[_], Key[_], Value[_]](using
      schema: EnrichedFieldSchemaInvariant[Self, Key, Value]
  ): EnrichedFieldSchemaInvariant[Self, Key, Value] = schema

  given [Self[_], Key[_], Value[_]](using
      self: FieldSchemaInvariant[Self, Key, Value],
      enrichment: EnrichedSchemaInvariant[Enrichment[Self, *]]
  ): EnrichedFieldSchemaInvariant[Enrichment[Self, *], Key, Value] =
    val field: FieldSchemaInvariant[Enrichment[Self, *], Key, Value] =
      self.imapK(Enrichment.liftK[Self])(Enrichment.unliftK[Self])

    new EnrichedFieldSchemaInvariant[Enrichment[Self, *], Key, Value]:
      export field.{apply, isOptional, key, nullish, optional, value}
      export enrichment.{imap, metadata}
