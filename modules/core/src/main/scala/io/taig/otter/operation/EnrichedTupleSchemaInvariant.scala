package io.taig.otter.operation

import io.taig.otter.Enrichment
import io.taig.otter.Metadata

trait EnrichedTupleSchemaInvariant[Self[_], -Value[_]]
    extends TupleSchemaInvariant[Self, Value],
      EnrichedSchemaInvariant[Self]:
  self =>

  override def imapK[T[_]](fK: [A] => Self[A] => T[A])(
      gK: [A] => T[A] => Self[A]
  ): EnrichedTupleSchemaInvariant[T, Value] =
    new EnrichedTupleSchemaInvariant[T, Value]:
      override def empty: T[Unit] = fK(self.empty)
      override def lift[A](schema: => Value[A]): T[A] = fK(self.lift(schema))

      extension [A](ta: T[A])
        override def zip[B](schema: T[B]): T[(A, B)] = fK(self.zip(gK(ta))(gK(schema)))

        override def metadata: Metadata = self.metadata(gK(ta))
        override def metadata(f: Metadata => Metadata): T[A] = fK(self.metadata(gK(ta))(f))

      override def imap[A, B](fa: T[A])(f: A => B)(g: B => A): T[B] = fK(self.imap(gK(fa))(f)(g))

object EnrichedTupleSchemaInvariant:
  inline def apply[Self[_], Field[_]](using
      schema: EnrichedTupleSchemaInvariant[Self, Field]
  ): EnrichedTupleSchemaInvariant[Self, Field] = schema

  given [Self[_], Value[_]](using
      self: TupleSchemaInvariant[Self, Value],
      enrichment: EnrichedSchemaInvariant[[a] =>> Enrichment[Self[a]]]
  ): EnrichedTupleSchemaInvariant[[a] =>> Enrichment[Self[a]], Value] =
    val tuple: TupleSchemaInvariant[[a] =>> Enrichment[Self[a]], Value] =
      self.imapK[[a] =>> Enrichment[Self[a]]](Enrichment.liftK[Self])(Enrichment.unliftK[Self])

    new EnrichedTupleSchemaInvariant[[a] =>> Enrichment[Self[a]], Value]:
      export tuple.{empty, lift, zip}
      export enrichment.{imap, metadata}
