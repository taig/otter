package io.taig.otter.http.codec

import cats.data.Chain
import cats.data.Validated
import io.taig.otter.Violations
import io.taig.otter.codec.CollectionDecoder
import io.taig.otter.codec.Decoder
import io.taig.otter.codec.TupleDecoder
import io.taig.otter.http.Parameter

object ParameterValueArrayDecoder extends Decoder[Parameter.Value.Array, Chain[String]]:
  val collection = CollectionDecoder(decoder = ParameterValueAtomParser)
  val tuple = TupleDecoder(decoder = ParameterValueAtomParser)

  override def decode[A](schema: Parameter.Value.Array[A], value: Chain[String]): Validated[Violations, A] =
    schema match
      case Parameter.Value.Array.Collection(self) => collection.decode(schema = self.self, value.toList)
      case Parameter.Value.Array.Tuple(self)      => tuple.decode(schema = self.self, value.toVector)
