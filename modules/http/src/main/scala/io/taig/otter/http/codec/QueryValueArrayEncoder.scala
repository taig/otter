package io.taig.otter.http.codec

import io.taig.otter.codec.CollectionEncoder
import io.taig.otter.codec.Encoder
import io.taig.otter.codec.TupleEncoder
import io.taig.otter.http.Query

object QueryValueArrayEncoder extends Encoder[Query.Schema.Array, Seq[String]]:
  val collection = CollectionEncoder(encoder = QueryValueAtomPrinter)
  val tuple = TupleEncoder(encoder = QueryValueAtomPrinter)

  override def encode[A](schema: Query.Schema.Array[A], a: A): Seq[String] = schema match
    case Query.Schema.Array.Collection(self) => collection.encode(schema = self.self, a)
    case Query.Schema.Array.Tuple(self)      => tuple.encode(schema = self.self, a)
