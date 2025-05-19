package io.taig.otter.http.codec

import cats.data.Chain
import cats.data.Validated
import io.taig.otter.Violations
import io.taig.otter.codec.CollectionDecoder
import io.taig.otter.codec.Decoder
import io.taig.otter.codec.TupleDecoder
import io.taig.otter.http.Http

object HttpParameterArrayDecoder extends Decoder[Http.Parameter.Array, Chain[String]]:
  val collection = CollectionDecoder(decoder = HttpParameterValueParser)
  val tuple = TupleDecoder(decoder = HttpParameterValueParser)

  override def decode[A](schema: Http.Parameter.Array[A], value: Chain[String]): Validated[Violations, A] = schema match
    case Http.Parameter.Array.Collection(self) => collection.decode(schema = self.self, value.toList)
    case Http.Parameter.Array.Tuple(self)      => tuple.decode(schema = self.self, value.toVector)
