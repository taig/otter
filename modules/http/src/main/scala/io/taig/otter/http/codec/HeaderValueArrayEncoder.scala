package io.taig.otter.http.codec

import cats.data.Chain
import io.taig.otter.codec.CollectionEncoder
import io.taig.otter.codec.Encoder
import io.taig.otter.codec.TupleEncoder
import io.taig.otter.http.Header

object HeaderValueArrayEncoder extends Encoder[Header.Value.Array, Chain[String]]:
  val collection = CollectionEncoder(encoder = HeaderValueAtomPrinter)
  val tuple = TupleEncoder(encoder = HeaderValueAtomPrinter)

  override def encode[A](schema: Header.Value.Array[A], a: A): Chain[String] = schema match
    case Header.Value.Array.Collection(self) => Chain.fromSeq(collection.encode(schema = self.self, a))
    case Header.Value.Array.Tuple(self)      => Chain.fromSeq(tuple.encode(schema = self.self, a))
