package io.taig.otter.http.codec

import cats.data.Chain
import io.taig.otter.codec.CollectionEncoder
import io.taig.otter.codec.Encoder
import io.taig.otter.codec.TupleEncoder
import io.taig.otter.http.Header

object HeaderSchemaArrayEncoder extends Encoder[Header.Schema.Array, Chain[String]]:
  val collection = CollectionEncoder(encoder = HeaderSchemaValuePrinter)
  val tuple = TupleEncoder(encoder = HeaderSchemaValuePrinter)

  override def encode[A](schema: Header.Schema.Array[A], a: A): Chain[String] = schema match
    case Header.Schema.Array.Collection(self) => Chain.fromSeq(collection.encode(schema = self, a))
    case Header.Schema.Array.Tuple(self)      => Chain.fromSeq(tuple.encode(schema = self, a))
