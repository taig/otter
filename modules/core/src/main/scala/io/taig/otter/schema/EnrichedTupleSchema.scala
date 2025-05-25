package io.taig.otter.schema

import io.taig.otter.Metadata
import io.taig.otter.Enrichment

trait EnrichedTupleSchema[Self[_], -Value[_]] extends TupleSchema[Self, Value], EnrichedSchema[Self]:
  self =>

  override def imapK[T[_]](fK: [A] => Self[A] => T[A])(gK: [A] => T[A] => Self[A]): EnrichedTupleSchema[T, Value] =
    new EnrichedTupleSchema[T, Value]:
      override def empty: T[Unit] = fK(self.empty)
      override def lift[A](schema: => Value[A]): T[A] = fK(self.lift(schema))

      extension [A](ta: T[A])
        override def zip[B](schema: T[B]): T[(A, B)] = fK(self.zip(gK(ta))(gK(schema)))

        override def metadata: Metadata = self.metadata(gK(ta))
        override def metadata(f: Metadata => Metadata): T[A] = fK(self.metadata(gK(ta))(f))

      override def imap[A, B](fa: T[A])(f: A => B)(g: B => A): T[B] = fK(self.imap(gK(fa))(f)(g))

object EnrichedTupleSchema:
  inline def apply[Self[_], Field[_]](using
      schema: EnrichedTupleSchema[Self, Field]
  ): EnrichedTupleSchema[Self, Field] = schema

  given [Self[_], Value[_]](using
      self: TupleSchema[Self, Value],
      enrichment: EnrichedSchema[Enrichment[Self, *]]
  ): EnrichedTupleSchema[Enrichment[Self, *], Value] =
    val tuple: TupleSchema[Enrichment[Self, *], Value] =
      self.imapK(Enrichment.liftK[Self])(Enrichment.unliftK[Self])

    new EnrichedTupleSchema[Enrichment[Self, *], Value]:
      export tuple.{empty, lift, zip}
      export enrichment.{imap, metadata}
