package io.taig.otter.schema

import io.taig.otter.Enrichment
import io.taig.otter.Metadata
import cats.Eq
import cats.syntax.all.*

trait EnrichedConstantSchema[Self[_], -Value[_]] extends ConstantSchema[Self, Value], EnrichedSchema[Self]:
  self =>

  override def imapK[T[_]](fK: [A] => Self[A] => T[A])(gK: [A] => T[A] => Self[A]): EnrichedConstantSchema[T, Value] =
    new EnrichedConstantSchema[T, Value]:
      override def apply[A: Eq](schema: => Value[A], value: A): T[Unit] = fK(self.apply(schema, value))

      extension [A](ta: T[A])
        override def metadata: Metadata = self.metadata(gK(ta))
        override def metadata(f: Metadata => Metadata): T[A] = fK(self.metadata(gK(ta))(f))

      override def imap[A, B](fa: T[A])(f: A => B)(g: B => A): T[B] = fK(self.imap(gK(fa))(f)(g))

object EnrichedConstantSchema:
  inline def apply[Self[_], Value[_]](using
      schema: EnrichedConstantSchema[Self, Value]
  ): EnrichedConstantSchema[Self, Value] = schema

  given [Self[_], Value[_]](using
      self: ConstantSchema[Self, Value],
      enrichment: EnrichedSchema[Enrichment[Self, *]]
  ): EnrichedConstantSchema[Enrichment[Self, *], Value] =
    val constant: ConstantSchema[Enrichment[Self, *], Value] =
      self.imapK(Enrichment.liftK[Self])(Enrichment.unliftK[Self])

    new EnrichedConstantSchema[Enrichment[Self, *], Value]:
      export constant.apply
      export enrichment.{imap, metadata}
