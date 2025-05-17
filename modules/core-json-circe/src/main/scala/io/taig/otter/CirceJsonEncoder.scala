package io.taig.otter

import cats.syntax.all.*
import io.circe.Json as CirceJson
import io.circe.syntax.*
import io.taig.otter.codec.Encoder
import io.taig.otter.codec.CollectionEncoder
import io.taig.otter.codec.ConstantEncoder
import io.taig.otter.codec.DictionaryEncoder
import io.taig.otter.codec.EnumerationEncoder
import io.taig.otter.codec.NullableEncoder
import io.taig.otter.codec.RecordEncoder
import io.taig.otter.codec.FieldEncoder
import io.taig.otter.codec.TupleEncoder
import io.taig.otter.codec.UnionEncoder
import io.taig.otter.codec.SumEncoder
import io.taig.otter.codec.BranchEncoder
import io.taig.otter.codec.KeyPrinter

object CirceJsonEncoder extends Encoder[Json, CirceJson]:
  val collection = CollectionEncoder(encoder = this)
  val constant = ConstantEncoder(encoder = this)
  val dictionary = DictionaryEncoder(key = KeyPrinter, value = this)
  val enumeration = EnumerationEncoder(encoder = this)
  val nullable = NullableEncoder(encoder = this, empty = CirceJson.Null)
  val record = RecordEncoder(
    field = FieldEncoder(key = KeyPrinter, value = this)
      .mapK[Json.Field]([A] => (field: Json.Field[A]) => field.self)
  )
  val sum = SumEncoder(branch =
    discriminator =>
      BranchEncoder(key = KeyPrinter, value = this)(discriminator)
        .mapK[Json.Branch]([A] => (branch: Json.Branch[A]) => branch.self)
  )
  val tuple = TupleEncoder(encoder = this)
  val union = UnionEncoder(encoder = this)

  override def encode[A](schema: Json[A], a: A): CirceJson = schema match
    case Json.Collection(self)  => CirceJson.fromValues(collection.encode(schema = self, a))
    case Json.Constant(self)    => constant.encode(schema = self, a)
    case Json.Dictionary(self)  => CirceJson.fromFields(dictionary.encode(schema = self, a))
    case Json.Enumeration(self) => enumeration.encode(schema = self, a)
    case Json.Nullable(self)    => nullable.encode(schema = self, a)
    case Json.Primitive(self)   => CirceJsonPrimitiveEncoder.encode(schema = self, a)
    case Json.Record(self)      => CirceJson.fromFields(record.encode(schema = self, a).toList)
    case Json.Sum(self)         => CirceJson.fromFields(sum.encode(schema = self, a).toList)
    case Json.Tuple(self)       => CirceJson.fromValues(tuple.encode(schema = self, a))
    case Json.Union(self)       => union.encode(schema = self, a)
