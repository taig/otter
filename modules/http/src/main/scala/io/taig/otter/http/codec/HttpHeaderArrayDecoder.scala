package io.taig.otter.http.codec

import io.taig.otter.codec.Encoder
import io.taig.otter.http.Http
import cats.data.Chain
import io.taig.otter.codec.CollectionEncoder
import io.taig.otter.codec.TupleEncoder
import io.taig.otter.codec.Decoder
import io.taig.otter.codec.CollectionDecoder
import io.taig.otter.codec.TupleDecoder
import cats.data.Validated
import io.taig.otter.Violations
import io.taig.otter.http.Http.Header

object HttpHeaderArrayDecoder extends Decoder[Http.Header.Array, Chain[String]]:
  val collection = CollectionDecoder(decoder = HttpHeaderValueParser)
  val tuple = TupleDecoder(decoder = HttpHeaderValueParser)

  override def decode[A](schema: Header.Array[A], value: Chain[String]): Validated[Violations, A] = schema match
    case Http.Header.Array.Collection(self) => collection.decode(schema = self, value.toList)
    case Http.Header.Array.Tuple(self)      => tuple.decode(schema = self, value.toVector)
