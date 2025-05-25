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
import io.taig.otter.http.Parameter

object ParameterValueObjectDecoder extends Decoder[Parameter.Value.Object, Chain[(String, Option[String])]]:
  val dictionary = DictionaryDecoder(key = KeyParser.Unquoted, value = HttpParameterObjectValueDecoder)
  val record = RecordDecoder(
    field = FieldDecoder(key = KeyCodec.Unquoted, value = HttpParameterObjectValueDecoder, empty = none[String])
      .mapK[Parameter.Value.Field]([A] => (field: Parameter.Value.Field[A]) => field.self.self)
  )

  override def decode[A](
      schema: Parameter.Value.Object[A],
      value: Chain[(String, Option[String])]
  ): Validated[Violations, A] = schema match
    case Parameter.Value.Object.Dictionary(self) => dictionary.decode(schema = self.self, value.toList)
    case Parameter.Value.Object.Record(self)     => record.decode(schema = self.self, value.toList)
