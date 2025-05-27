package io.taig.otter.http.codec

import cats.data.Chain
import cats.data.Validated
import io.taig.otter.Violations
import io.taig.otter.codec.CollectionDecoder
import io.taig.otter.codec.Decoder
import io.taig.otter.codec.TupleDecoder
import io.taig.otter.http.Parameter

object ParameterSchemaArrayDecoder extends Decoder[Parameter.Schema.Array, Chain[String]]:
  val collection = CollectionDecoder(decoder = ParameterSchemaValueParser)
  val tuple = TupleDecoder(decoder = ParameterSchemaValueParser)

  override def decode[A](schema: Parameter.Schema.Array[A], value: Chain[String]): Validated[Violations, A] =
    schema match
      case Parameter.Schema.Array.Collection(self) => collection.decode(schema = self.self, value.toList)
      case Parameter.Schema.Array.Tuple(self)      => tuple.decode(schema = self.self, value.toVector)
