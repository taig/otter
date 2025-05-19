package io.taig.otter.http.codec

import cats.data.Chain
import io.taig.otter.codec.CollectionEncoder
import io.taig.otter.codec.Encoder
import io.taig.otter.codec.TupleEncoder
import io.taig.otter.http.Http
import io.taig.otter.http.Http.Parameter
import io.taig.otter.http.Http.Parameter.Array.Collection

object HttpParameterArrayEncoder extends Encoder[Http.Parameter.Array, Chain[String]]:
  val collection = CollectionEncoder(encoder = HttpParameterValuePrinter)
  val tuple = TupleEncoder(encoder = HttpParameterValuePrinter)

  override def encode[A](schema: Parameter.Array[A], a: A): Chain[String] = schema match
    case Http.Parameter.Array.Collection(self) => Chain.fromSeq(collection.encode(schema = self, a))
    case Http.Parameter.Array.Tuple(self)      => Chain.fromSeq(tuple.encode(schema = self, a))
