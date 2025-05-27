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

object ParameterSchemaObjectDecoder extends Decoder[Parameter.Schema.Object, Chain[(String, Option[String])]]:
  val dictionary = DictionaryDecoder(key = KeyParser.Unquoted, value = ParameterSchemaObjectValueDecoder)
  val record = RecordDecoder(
    field = FieldDecoder(key = KeyCodec.Unquoted, value = ParameterSchemaObjectValueDecoder, empty = none[String])
      .mapK[Parameter.Schema.Field]([A] => (field: Parameter.Schema.Field[A]) => field.self.self)
  )

  override def decode[A](
      schema: Parameter.Schema.Object[A],
      value: Chain[(String, Option[String])]
  ): Validated[Violations, A] = schema match
    case Parameter.Schema.Object.Dictionary(self) => dictionary.decode(schema = self.self, value.toList)
    case Parameter.Schema.Object.Record(self)     => record.decode(schema = self.self, value.toList)
