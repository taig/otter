package io.taig.otter.operation

import io.taig.otter.Metadata
import io.taig.otter.Enrichment
import cats.syntax.all.*

trait EnrichedDictionarySchemaInvariant[Self[_], -Key[_], -Value[_]]
    extends DictionarySchemaInvariant[Self, Key, Value],
      EnrichedSchemaInvariant[Self]:
  self =>

  override def imapK[T[_]](fK: [A] => Self[A] => T[A])(
      gK: [A] => T[A] => Self[A]
  ): EnrichedDictionarySchemaInvariant[T, Key, Value] =
    new EnrichedDictionarySchemaInvariant[T, Key, Value]:

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

object EnrichedDictionarySchemaInvariant:
  inline def apply[Self[_], Key[_], Value[_]](using
      schema: EnrichedDictionarySchemaInvariant[Self, Key, Value]
  ): EnrichedDictionarySchemaInvariant[Self, Key, Value] = schema

  given [Self[_], Key[_], Value[_]](using
      self: DictionarySchemaInvariant[Self, Key, Value],
      enrichment: EnrichedSchemaInvariant[[a] =>> Enrichment[Self[a]]]
  ): EnrichedDictionarySchemaInvariant[[a] =>> Enrichment[Self[a]], Key, Value] =
    val dictionary: DictionarySchemaInvariant[[a] =>> Enrichment[Self[a]], Key, Value] =
      self.imapK[[a] =>> Enrichment[Self[a]]](Enrichment.liftK[Self])(Enrichment.unliftK[Self])

    new EnrichedDictionarySchemaInvariant[[a] =>> Enrichment[Self[a]], Key, Value]:
      export dictionary.apply
      export enrichment.{imap, metadata}
