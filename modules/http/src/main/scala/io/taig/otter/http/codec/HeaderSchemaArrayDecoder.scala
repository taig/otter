package io.taig.otter.http.codec

import cats.data.Chain
import cats.data.Validated
import io.taig.otter.Violations
import io.taig.otter.codec.CollectionDecoder
import io.taig.otter.codec.Decoder
import io.taig.otter.codec.TupleDecoder
import io.taig.otter.http.Header

object HeaderSchemaArrayDecoder extends Decoder[Header.Schema.Array, Chain[String]]:
  val collection = CollectionDecoder(decoder = HeaderSchemaValueParser)
  val tuple = TupleDecoder(decoder = HeaderSchemaValueParser)

  override def decode[A](schema: Header.Schema.Array[A], value: Chain[String]): Validated[Violations, A] = schema match
    case Header.Schema.Array.Collection(self) => collection.decode(schema = self.self, value.toList)
    case Header.Schema.Array.Tuple(self)      => tuple.decode(schema = self.self, value.toVector)
