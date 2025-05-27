package io.taig.otter.http.codec

import cats.data.Chain
import cats.data.Validated
import cats.syntax.all.*
import io.taig.otter.Violations
import io.taig.otter.codec.Decoder
import io.taig.otter.codec.DictionaryDecoder
import io.taig.otter.codec.FieldDecoder
import io.taig.otter.codec.KeyCodec
import io.taig.otter.codec.KeyParser
import io.taig.otter.codec.RecordDecoder
import io.taig.otter.http.Header

object HeaderSchemaObjectDecoder extends Decoder[Header.Schema.Object, Chain[(String, Option[String])]]:
  val dictionary = DictionaryDecoder(key = KeyParser.Unquoted, value = HeaderSchemaObjectValueDecoder)
  val record = RecordDecoder(
    field = FieldDecoder(key = KeyCodec.Unquoted, value = HeaderSchemaObjectValueDecoder, empty = none[String])
      .mapK[Header.Schema.Field]([A] => (field: Header.Schema.Field[A]) => field.self.self)
  )

  override def decode[A](
      schema: Header.Schema.Object[A],
      value: Chain[(String, Option[String])]
  ): Validated[Violations, A] = schema match
    case Header.Schema.Object.Dictionary(self) => dictionary.decode(schema = self.self, value.toList)
    case Header.Schema.Object.Record(self)     => record.decode(schema = self.self, value.toList)
