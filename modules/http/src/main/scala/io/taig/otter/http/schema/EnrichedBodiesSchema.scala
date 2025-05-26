package io.taig.otter.http.schema

import io.taig.otter.Enrichment
import io.taig.otter.schema.EnrichedSchema

trait EnrichedBodiesSchema[Self[+_[_], _], Body[+_[_], _]] extends BodiesSchema[Self, Body], EnrichedSchemaK[Self]:
  override def algebra[S[_]]: EnrichedSchema[Self[S, *]]

object EnrichedBodiesSchema:
  inline def apply[Self[+_[_], _], Body[+_[_], _]](using
      schema: EnrichedBodiesSchema[Self, Body]
  ): EnrichedBodiesSchema[Self, Body] = schema

  given [Self[+_[_], _], Body[+_[_], _]](using
      schema: BodiesSchema[Self, Body],
      enrichment: EnrichedSchemaK[[s[_], a] =>> Enrichment[Self[s, *], a]]
  ): EnrichedBodiesSchema[[s[_], a] =>> Enrichment[Self[s, *], a], Body] =
    val body: BodiesSchema[[s[_], a] =>> Enrichment[Self[s, *], a], Body] = schema
      .imapK[[s[_], a] =>> Enrichment[Self[s, *], a]](
        [S[_], A] => (self: Self[S, A]) => Enrichment(self)
      )([S[_], A] => (value: Enrichment[Self[S, *], A]) => value.self)

    new EnrichedBodiesSchema[[s[_], a] =>> Enrichment[Self[s, *], a], Body]:
      export body.{apply, orElse}
      export enrichment.algebra
