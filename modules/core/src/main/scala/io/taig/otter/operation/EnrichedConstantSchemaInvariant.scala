package io.taig.otter.operation

import io.taig.otter.Enrichment
import io.taig.otter.Metadata
import cats.Eq
import cats.syntax.all.*

trait EnrichedConstantSchemaInvariant[Self[_], -Value[_]]
    extends ConstantSchemaInvariant[Self, Value],
      EnrichedSchemaInvariant[Self]:
  self =>

  override def imapK[T[_]](fK: [A] => Self[A] => T[A])(
      gK: [A] => T[A] => Self[A]
  ): EnrichedConstantSchemaInvariant[T, Value] =
    new EnrichedConstantSchemaInvariant[T, Value]:
      override def apply[A: Eq](schema: => Value[A], value: A): T[Unit] = fK(self.apply(schema, value))

      extension [A](ta: T[A])
        override def metadata: Metadata = self.metadata(gK(ta))
        override def metadata(f: Metadata => Metadata): T[A] = fK(self.metadata(gK(ta))(f))

      override def imap[A, B](fa: T[A])(f: A => B)(g: B => A): T[B] = fK(self.imap(gK(fa))(f)(g))

object EnrichedConstantSchemaInvariant:
  inline def apply[Self[_], Value[_]](using
      schema: EnrichedConstantSchemaInvariant[Self, Value]
  ): EnrichedConstantSchemaInvariant[Self, Value] = schema

  given [Self[_], Value[_]](using
      self: ConstantSchemaInvariant[Self, Value],
      enrichment: EnrichedSchemaInvariant[[a] =>> Enrichment[Self[a]]]
  ): EnrichedConstantSchemaInvariant[[a] =>> Enrichment[Self[a]], Value] =
    val constant: ConstantSchemaInvariant[[a] =>> Enrichment[Self[a]], Value] =
      self.imapK[[a] =>> Enrichment[Self[a]]](Enrichment.liftK[Self])(Enrichment.unliftK[Self])

    new EnrichedConstantSchemaInvariant[[a] =>> Enrichment[Self[a]], Value]:
      export constant.apply
      export enrichment.{imap, metadata}
