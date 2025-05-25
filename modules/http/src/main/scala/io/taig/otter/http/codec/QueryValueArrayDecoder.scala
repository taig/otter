package io.taig.otter.http.codec

import cats.data.Chain
import cats.data.Validated
import io.taig.otter.Violations
import io.taig.otter.codec.CollectionDecoder
import io.taig.otter.codec.Decoder
import io.taig.otter.codec.TupleDecoder
import io.taig.otter.http.Query

object QueryValueArrayDecoder extends Decoder[Query.Value.Array, Chain[String]]:
  val collection = CollectionDecoder(decoder = QueryValueAtomParser)
  val tuple = TupleDecoder(decoder = QueryValueAtomParser)

  override def decode[A](schema: Query.Value.Array[A], values: Chain[String]): Validated[Violations, A] = schema match
    case Query.Value.Array.Collection(self) => collection.decode(schema = self.self, values = values.toList)
    case Query.Value.Array.Tuple(self)      => tuple.decode(schema = self.self, values = values.toList)
