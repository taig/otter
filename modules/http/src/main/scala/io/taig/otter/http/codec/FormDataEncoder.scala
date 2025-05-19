package io.taig.otter.http.codec

import io.taig.otter.codec.DictionaryEncoder
import io.taig.otter.codec.Encoder
import io.taig.otter.codec.FieldEncoder
import io.taig.otter.codec.KeyPrinter
import io.taig.otter.codec.RecordEncoder
import io.taig.otter.http.FormData
import io.taig.otter.http.FormData.Dictionary

object FormDataEncoder extends Encoder[FormData, List[(String, Option[String])]]:
  val dictionary = DictionaryEncoder(key = KeyPrinter.Unquoted, value = FormDataValuePrinter)
  val record = RecordEncoder(
    field = FieldEncoder(key = KeyPrinter.Unquoted, value = FormDataValuePrinter)
      .mapK[FormData.Field]([A] => (field: FormData.Field[A]) => field.self)
  )

  override def encode[A](schema: FormData[A], a: A): List[(String, Option[String])] = schema match
    case FormData.Dictionary(self) => dictionary.encode(schema = self, a)
    case FormData.Record(self)     => record.encode(schema = self, a).toList
