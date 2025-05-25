package io.taig.otter.http.schema

import io.taig.otter.Enrichment
import io.taig.otter.schema.EnrichedSchema
import io.taig.otter.Reference
import io.taig.otter.http.header.MediaType
import io.taig.otter.Metadata
import cats.syntax.all.*

trait EnrichedBodySchema[Self[+_[_], _]] extends BodySchema[Self], EnrichedSchemaK[Self]:
  override def algebra[S[_]]: EnrichedSchema[Self[S, *]]

object EnrichedBodySchema:
  inline def apply[Self[+_[_], _]](using schema: EnrichedBodySchema[Self]): EnrichedBodySchema[Self] = schema

  given [Self[+_[_], _]](using
      schema: BodySchema[Self],
      enrichment: EnrichedSchemaK[[s[_], a] =>> Enrichment[Self[s, *], a]]
  ): EnrichedBodySchema[[s[_], a] =>> Enrichment[Self[s, *], a]] =
    val body: BodySchema[[s[_], a] =>> Enrichment[Self[s, *], a]] = schema
      .imapK[[s[_], a] =>> Enrichment[Self[s, *], a]](
        [S[_], A] => (self: Self[S, A]) => Enrichment(self)
      )([S[_], A] => (value: Enrichment[Self[S, *], A]) => value.self)

    new EnrichedBodySchema[[s[_], a] =>> Enrichment[Self[s, *], a]]:
      export body.{apply, mediaType, schema}
      export enrichment.algebra
