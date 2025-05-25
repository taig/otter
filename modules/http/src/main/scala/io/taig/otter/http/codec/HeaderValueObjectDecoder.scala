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

object HeaderValueObjectDecoder extends Decoder[Header.Value.Object, Chain[(String, Option[String])]]:
  val dictionary = DictionaryDecoder(key = KeyParser.Unquoted, value = HeaderValueObjectValueDecoder)
  val record = RecordDecoder(
    field = FieldDecoder(key = KeyCodec.Unquoted, value = HeaderValueObjectValueDecoder, empty = none[String])
      .mapK[Header.Value.Field]([A] => (field: Header.Value.Field[A]) => field.self.self)
  )

  override def decode[A](
      schema: Header.Value.Object[A],
      value: Chain[(String, Option[String])]
  ): Validated[Violations, A] =
    schema match
      case Header.Value.Object.Dictionary(self) => dictionary.decode(schema = self.self, value.toList)
      case Header.Value.Object.Record(self)     => record.decode(schema = self.self, value.toList)
