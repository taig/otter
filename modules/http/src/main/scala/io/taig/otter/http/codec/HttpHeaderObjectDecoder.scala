package io.taig.otter.http.codec

import io.taig.otter.codec.Encoder
import io.taig.otter.http.Http
import cats.syntax.all.*
import cats.data.Chain
import io.taig.otter.codec.DictionaryEncoder
import io.taig.otter.codec.KeyPrinter
import io.taig.otter.http.Http.Header.Object.Dictionary
import io.taig.otter.codec.RecordEncoder
import io.taig.otter.codec.FieldEncoder
import io.taig.otter.codec.Decoder
import io.taig.otter.codec.DictionaryDecoder
import io.taig.otter.codec.RecordDecoder
import cats.data.Validated
import io.taig.otter.Violations
import io.taig.otter.http.Http.Header
import io.taig.otter.codec.KeyParser
import io.taig.otter.codec.FieldDecoder
import io.taig.otter.codec.KeyCodec

object HttpHeaderObjectDecoder extends Decoder[Http.Header.Object, Chain[(String, Option[String])]]:
  val dictionary = DictionaryDecoder(key = KeyParser, value = HttpHeaderObjectValueDecoder)
  val record = RecordDecoder(
    field = FieldDecoder(key = KeyCodec, value = HttpHeaderObjectValueDecoder, empty = none)
      .mapK[Http.Header.Field]([A] => (field: Http.Header.Field[A]) => field.self)
  )

  override def decode[A](schema: Header.Object[A], value: Chain[(String, Option[String])]): Validated[Violations, A] =
    schema match
      case Http.Header.Object.Dictionary(self) => dictionary.decode(schema = self, value.toList)
      case Http.Header.Object.Record(self)     => record.decode(schema = self, value.toList)
