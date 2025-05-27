package io.taig.otter.http.codec

import cats.data.Chain
import io.taig.otter.codec.DictionaryEncoder
import io.taig.otter.codec.Encoder
import io.taig.otter.codec.FieldEncoder
import io.taig.otter.codec.KeyPrinter
import io.taig.otter.codec.RecordEncoder
import io.taig.otter.http.Parameter

object ParameterSchemaObjectEncoder extends Encoder[Parameter.Schema.Object, Chain[(String, Option[String])]]:
  val dictionary = DictionaryEncoder(key = KeyPrinter.Unquoted, value = ParameterSchemaObjectValueEncoder)
  val record = RecordEncoder(field =
    FieldEncoder(key = KeyPrinter.Unquoted, value = ParameterSchemaObjectValueEncoder)
      .mapK[Parameter.Schema.Field]([A] => (field: Parameter.Schema.Field[A]) => field.self.self)
  )

  override def encode[A](schema: Parameter.Schema.Object[A], a: A): Chain[(String, Option[String])] = schema match
    case Parameter.Schema.Object.Dictionary(self) => Chain.fromSeq(dictionary.encode(schema = self.self, a))
    case Parameter.Schema.Object.Record(self)     => record.encode(schema = self.self, a)
