package io.taig.otter.http.codec

import cats.data.Chain
import cats.data.Validated
import io.taig.otter.Violations
import io.taig.otter.codec.CollectionDecoder
import io.taig.otter.codec.Decoder
import io.taig.otter.codec.TupleDecoder
import io.taig.otter.http.Header

object HeaderValueArrayDecoder extends Decoder[Header.Value.Array, Chain[String]]:
  val collection = CollectionDecoder(decoder = HeaderValueAtomParser)
  val tuple = TupleDecoder(decoder = HeaderValueAtomParser)

  override def decode[A](schema: Header.Value.Array[A], value: Chain[String]): Validated[Violations, A] = schema match
    case Header.Value.Array.Collection(self) => collection.decode(schema = self.self, value.toList)
    case Header.Value.Array.Tuple(self)      => tuple.decode(schema = self.self, value.toVector)
