package io.taig.otter.http.codec

import cats.data.Chain
import io.taig.otter.codec.CollectionEncoder
import io.taig.otter.codec.Encoder
import io.taig.otter.codec.TupleEncoder
import io.taig.otter.http.Parameter

object ParameterSchemaArrayEncoder extends Encoder[Parameter.Schema.Array, Chain[String]]:
  val collection = CollectionEncoder(encoder = ParameterSchemaAtomPrinter)
  val tuple = TupleEncoder(encoder = ParameterSchemaAtomPrinter)

  override def encode[A](schema: Parameter.Schema.Array[A], a: A): Chain[String] = schema match
    case Parameter.Schema.Array.Collection(self) => Chain.fromSeq(collection.encode(schema = self.self, a))
    case Parameter.Schema.Array.Tuple(self)      => Chain.fromSeq(tuple.encode(schema = self.self, a))
