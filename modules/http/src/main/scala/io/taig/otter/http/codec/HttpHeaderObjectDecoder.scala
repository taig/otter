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
import io.taig.otter.http.Http
import io.taig.otter.http.Http.Header
import io.taig.otter.http.Http.Header.Object.Dictionary

object HttpHeaderObjectDecoder extends Decoder[Http.Header.Object, Chain[(String, Option[String])]]:
  val dictionary = DictionaryDecoder(key = KeyParser.Unquoted, value = HttpHeaderObjectValueDecoder)
  val record = RecordDecoder(
    field = FieldDecoder(key = KeyCodec.Unquoted, value = HttpHeaderObjectValueDecoder, empty = none[String])
      .mapK[Http.Header.Field]([A] => (field: Http.Header.Field[A]) => field.self)
  )

  override def decode[A](schema: Header.Object[A], value: Chain[(String, Option[String])]): Validated[Violations, A] =
    schema match
      case Http.Header.Object.Dictionary(self) => dictionary.decode(schema = self, value.toList)
      case Http.Header.Object.Record(self)     => record.decode(schema = self, value.toList)
