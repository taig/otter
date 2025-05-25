package io.taig.otter.http.codec

import cats.data.Chain
import io.taig.otter.codec.DictionaryEncoder
import io.taig.otter.codec.Encoder
import io.taig.otter.codec.FieldEncoder
import io.taig.otter.codec.KeyPrinter
import io.taig.otter.codec.RecordEncoder
import io.taig.otter.http.Parameter

object ParameterValueObjectEncoder extends Encoder[Parameter.Value.Object, Chain[(String, Option[String])]]:
  val dictionary = DictionaryEncoder(key = KeyPrinter.Unquoted, value = HttpParameterObjectValueEncoder)
  val record = RecordEncoder(field =
    FieldEncoder(key = KeyPrinter.Unquoted, value = HttpParameterObjectValueEncoder)
      .mapK[Parameter.Value.Field]([A] => (field: Parameter.Value.Field[A]) => field.self.self)
  )

  override def encode[A](schema: Parameter.Value.Object[A], a: A): Chain[(String, Option[String])] = schema match
    case Parameter.Value.Object.Dictionary(self) => Chain.fromSeq(dictionary.encode(schema = self.self, a))
    case Parameter.Value.Object.Record(self)     => record.encode(schema = self.self, a)
