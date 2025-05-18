package io.taig.otter.http.codec

import cats.data.Chain
import io.taig.otter.codec.CollectionEncoder
import io.taig.otter.codec.Encoder
import io.taig.otter.codec.TupleEncoder
import io.taig.otter.http.Http

object HttpHeaderArrayEncoder extends Encoder[Http.Header.Array, Chain[String]]:
  val collection = CollectionEncoder(encoder = HttpHeaderValuePrinter)
  val tuple = TupleEncoder(encoder = HttpHeaderValuePrinter)

  override def encode[A](schema: Http.Header.Array[A], a: A): Chain[String] = schema match
    case Http.Header.Array.Collection(self) => Chain.fromSeq(collection.encode(schema = self, a))
    case Http.Header.Array.Tuple(self)      => Chain.fromSeq(tuple.encode(schema = self, a))
