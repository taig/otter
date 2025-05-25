package io.taig.otter.schema

import io.taig.otter.Metadata
import io.taig.otter.Enrichment
import cats.syntax.all.*

trait EnrichedDictionarySchema[Self[_], -Key[_], -Value[_]]
    extends DictionarySchema[Self, Key, Value],
      EnrichedSchema[Self]:
  self =>

  override def imapK[T[_]](fK: [A] => Self[A] => T[A])(
      gK: [A] => T[A] => Self[A]
  ): EnrichedDictionarySchema[T, Key, Value] =
    new EnrichedDictionarySchema[T, Key, Value]:

      override def apply[A, B](
          key: => Key[A],
          value: => Value[B],
          minimum: Option[Int],
          maximum: Option[Int]
      ): T[List[(A, B)]] =
        fK(self(key, value, minimum, maximum))

      extension [A](ta: T[A])
        override def metadata: Metadata = self.metadata(gK(ta))
        override def metadata(f: Metadata => Metadata): T[A] = fK(self.metadata(gK(ta))(f))

      override def imap[A, B](fa: T[A])(f: A => B)(g: B => A): T[B] = fK(self.imap(gK(fa))(f)(g))

object EnrichedDictionarySchema:
  inline def apply[Self[_], Key[_], Value[_]](using
      schema: EnrichedDictionarySchema[Self, Key, Value]
  ): EnrichedDictionarySchema[Self, Key, Value] = schema

  given [Self[_], Key[_], Value[_]](using
      self: DictionarySchema[Self, Key, Value],
      enrichment: EnrichedSchema[Enrichment[Self, *]]
  ): EnrichedDictionarySchema[Enrichment[Self, *], Key, Value] =
    val dictionary: DictionarySchema[Enrichment[Self, *], Key, Value] =
      self.imapK(Enrichment.liftK[Self])(Enrichment.unliftK[Self])

    new EnrichedDictionarySchema[Enrichment[Self, *], Key, Value]:
      export dictionary.apply
      export enrichment.{imap, metadata}
