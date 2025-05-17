package io.taig.otter.http.codec

import io.taig.otter.codec.Decoder
import io.taig.otter.http.Http
import cats.data.Validated
import io.taig.otter.Violations
import io.taig.otter.codec.CollectionDecoder
import io.taig.otter.codec.TupleDecoder
import cats.data.Chain

object HttpQueryArrayDecoder extends Decoder[Http.Query.Array, Chain[String]]:
  val collection = CollectionDecoder(decoder = HttpQueryValueParser)
  val tuple = TupleDecoder(decoder = HttpQueryValueParser)

  override def decode[A](schema: Http.Query.Array[A], values: Chain[String]): Validated[Violations, A] = schema match
    case Http.Query.Array.Collection(self) => collection.decode(schema = self, values = values.toList)
    case Http.Query.Array.Tuple(self)      => tuple.decode(schema = self, values = values.toList)
