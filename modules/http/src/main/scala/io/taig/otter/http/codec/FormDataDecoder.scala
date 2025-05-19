package io.taig.otter.http.codec

import cats.data.Validated
import cats.syntax.all.*
import io.taig.otter.Violations
import io.taig.otter.codec.Decoder
import io.taig.otter.codec.DictionaryDecoder
import io.taig.otter.codec.FieldDecoder
import io.taig.otter.codec.KeyCodec
import io.taig.otter.codec.KeyParser
import io.taig.otter.codec.RecordDecoder
import io.taig.otter.http.FormData
import io.taig.otter.http.FormData.Dictionary

object FormDataDecoder extends Decoder[FormData, List[(String, Option[String])]]:
  val dictionary = DictionaryDecoder(key = KeyParser.Unquoted, value = FormDataValueParser)
  val record = RecordDecoder(
    field = FieldDecoder(key = KeyCodec.Unquoted, value = FormDataValueParser, empty = none[String])
      .mapK[FormData.Field]([A] => (field: FormData.Field[A]) => field.self)
  )

  override def decode[A](schema: FormData[A], value: List[(String, Option[String])]): Validated[Violations, A] =
    schema match
      case FormData.Dictionary(self) => dictionary.decode(schema = self, value)
      case FormData.Record(self)     => record.decode(schema = self, value)
