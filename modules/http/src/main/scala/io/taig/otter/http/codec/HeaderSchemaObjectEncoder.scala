package io.taig.otter.http.codec

import cats.data.Chain
import io.taig.otter.codec.DictionaryEncoder
import io.taig.otter.codec.Encoder
import io.taig.otter.codec.FieldEncoder
import io.taig.otter.codec.KeyPrinter
import io.taig.otter.codec.RecordEncoder
import io.taig.otter.http.Header

object HeaderSchemaObjectEncoder extends Encoder[Header.Schema.Object, Chain[(String, Option[String])]]:
  val dictionary = DictionaryEncoder(key = KeyPrinter.Unquoted, value = HeaderSchemaObjectValueEncoder)
  val record = RecordEncoder(
    field = FieldEncoder(key = KeyPrinter.Unquoted, value = HeaderSchemaObjectValueEncoder)
      .mapK[Header.Schema.Field]([A] => (field: Header.Schema.Field[A]) => field.self)
  )

  override def encode[A](schema: Header.Schema.Object[A], a: A): Chain[(String, Option[String])] = schema match
    case Header.Schema.Object.Dictionary(self) => Chain.fromSeq(dictionary.encode(schema = self, a))
    case Header.Schema.Object.Record(self)     => record.encode(schema = self, a)
