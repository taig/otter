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

object HttpParameterObjectDecoder extends Decoder[Http.Parameter.Object, Chain[(String, Option[String])]]:
  val dictionary = DictionaryDecoder(key = KeyParser.Unquoted, value = HttpParameterObjectValueDecoder)
  val record = RecordDecoder(
    field = FieldDecoder(key = KeyCodec.Unquoted, value = HttpParameterObjectValueDecoder, empty = none[String])
      .mapK[Http.Parameter.Field]([A] => (field: Http.Parameter.Field[A]) => field.self)
  )

  override def decode[A](
      schema: Http.Parameter.Object[A],
      value: Chain[(String, Option[String])]
  ): Validated[Violations, A] =
    schema match
      case Http.Parameter.Object.Dictionary(self) => dictionary.decode(schema = self, value.toList)
      case Http.Parameter.Object.Record(self)     => record.decode(schema = self, value.toList)
