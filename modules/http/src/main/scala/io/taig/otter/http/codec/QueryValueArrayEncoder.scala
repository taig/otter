package io.taig.otter.http.codec

import io.taig.otter.codec.CollectionEncoder
import io.taig.otter.codec.Encoder
import io.taig.otter.codec.TupleEncoder
import io.taig.otter.http.Query

object QueryValueArrayEncoder extends Encoder[Query.Value.Array, Seq[String]]:
  val collection = CollectionEncoder(encoder = QueryValueAtomPrinter)
  val tuple = TupleEncoder(encoder = QueryValueAtomPrinter)

  override def encode[A](schema: Query.Value.Array[A], a: A): Seq[String] = schema match
    case Query.Value.Array.Collection(self) => collection.encode(schema = self.self, a)
    case Query.Value.Array.Tuple(self)      => tuple.encode(schema = self.self, a)
