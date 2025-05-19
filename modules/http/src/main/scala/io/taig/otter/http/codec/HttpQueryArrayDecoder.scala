package io.taig.otter.http.codec

import cats.data.Chain
import cats.data.Validated
import io.taig.otter.Violations
import io.taig.otter.codec.CollectionDecoder
import io.taig.otter.codec.Decoder
import io.taig.otter.codec.TupleDecoder
import io.taig.otter.http.Http

object HttpQueryArrayDecoder extends Decoder[Http.Query.Array, Chain[String]]:
  val collection = CollectionDecoder(decoder = HttpQueryValueParser)
  val tuple = TupleDecoder(decoder = HttpQueryValueParser)

  override def decode[A](schema: Http.Query.Array[A], values: Chain[String]): Validated[Violations, A] = schema match
    case Http.Query.Array.Collection(self) => collection.decode(schema = self.self, values = values.toList)
    case Http.Query.Array.Tuple(self)      => tuple.decode(schema = self.self, values = values.toList)
