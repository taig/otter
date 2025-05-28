package io.taig.otter.codec

import io.circe.Json as CirceJson
import io.taig.otter.Json

object CirceJsonEncoder extends Encoder[Json, CirceJson]:
  val collection = CollectionEncoder(encoder = this)
  val constant = ConstantEncoder(encoder = this)
  val dictionary = DictionaryEncoder(key = KeyPrinter.Unquoted, value = this)
  val enumeration = EnumerationEncoder(encoder = this)
  val nullable = NullableEncoder(encoder = this, empty = CirceJson.Null)
  val record = RecordEncoder(
    field = FieldEncoder(key = KeyPrinter.Unquoted, value = this)
      .mapK[Json.Field]([A] => (field: Json.Field[A]) => field.self)
  )
  val tuple = TupleEncoder(encoder = this)
  val union = UnionEncoder(encoder = this)

  override def encode[A](schema: Json[A], a: A): CirceJson = schema match
    case Json.Collection(self)  => CirceJson.fromValues(collection.encode(schema = self, a))
    case Json.Constant(self)    => constant.encode(schema = self, a)
    case Json.Dictionary(self)  => CirceJson.fromFields(dictionary.encode(schema = self, a))
    case Json.Enumeration(self) => enumeration.encode(schema = self, a)
    case Json.Nullable(self)    => nullable.encode(schema = self, a)
    case Json.Primitive(self)   => CirceJsonPrimitiveEncoder.encode(schema = self.value, a)
    case Json.Record(self)      => CirceJson.fromFields(record.encode(schema = self, a).toList)
    case Json.Tuple(self)       => CirceJson.fromValues(tuple.encode(schema = self, a))
    case Json.Union(self)       => union.encode(schema = self, a)
