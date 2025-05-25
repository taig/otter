package io.taig.otter.http.codec

import cats.data.Chain
import io.taig.otter.codec.CollectionEncoder
import io.taig.otter.codec.Encoder
import io.taig.otter.codec.TupleEncoder
import io.taig.otter.http.Parameter

object ParameterValueArrayEncoder extends Encoder[Parameter.Value.Array, Chain[String]]:
  val collection = CollectionEncoder(encoder = ParameterValueAtomPrinter)
  val tuple = TupleEncoder(encoder = ParameterValueAtomPrinter)

  override def encode[A](schema: Parameter.Value.Array[A], a: A): Chain[String] = schema match
    case Parameter.Value.Array.Collection(self) => Chain.fromSeq(collection.encode(schema = self.self, a))
    case Parameter.Value.Array.Tuple(self)      => Chain.fromSeq(tuple.encode(schema = self.self, a))
