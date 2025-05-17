package io.taig.otter.http.codec

import io.taig.otter.codec.Decoder
import io.taig.otter.http.FormData
import cats.data.Validated
import io.taig.otter.Violations
import io.taig.otter.http.FormData.Dictionary
import io.taig.otter.codec.DictionaryDecoder
import io.taig.otter.codec.KeyParser
import io.taig.otter.codec.RecordDecoder
import io.taig.otter.codec.FieldDecoder
import io.taig.otter.codec.KeyCodec
import cats.syntax.all.*

object FormDataDecoder extends Decoder[FormData, List[(String, Option[String])]]:
  val dictionary = DictionaryDecoder(key = KeyParser, value = FormDataValueParser)
  val record = RecordDecoder(
    field = FieldDecoder(key = KeyCodec, value = FormDataValueParser, empty = none)
      .mapK[FormData.Field]([A] => (field: FormData.Field[A]) => field.self)
  )

  override def decode[A](schema: FormData[A], value: List[(String, Option[String])]): Validated[Violations, A] =
    schema match
      case FormData.Dictionary(self) => dictionary.decode(schema = self, value)
      case FormData.Record(self)     => record.decode(schema = self, value)
