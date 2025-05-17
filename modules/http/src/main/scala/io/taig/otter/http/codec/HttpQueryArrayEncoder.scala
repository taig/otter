package io.taig.otter.http.codec

import io.taig.otter.http.Http
import io.taig.otter.codec.Encoder
import io.taig.otter.codec.CollectionEncoder
import io.taig.otter.codec.TupleEncoder

object HttpQueryArrayEncoder extends Encoder[Http.Query.Array, Seq[String]]:
  val collection = CollectionEncoder(encoder = HttpQueryValuePrinter)
  val tuple = TupleEncoder(encoder = HttpQueryValuePrinter)

  override def encode[A](schema: Http.Query.Array[A], a: A): Seq[String] = schema match
    case Http.Query.Array.Collection(self) => collection.encode(schema = self, a)
    case Http.Query.Array.Tuple(self)      => tuple.encode(schema = self, a)
