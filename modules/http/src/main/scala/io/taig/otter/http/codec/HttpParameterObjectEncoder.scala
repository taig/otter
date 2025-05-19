package io.taig.otter.http.codec

import cats.data.Chain
import io.taig.otter.codec.DictionaryEncoder
import io.taig.otter.codec.Encoder
import io.taig.otter.codec.FieldEncoder
import io.taig.otter.codec.KeyPrinter
import io.taig.otter.codec.RecordEncoder
import io.taig.otter.http.Http

object HttpParameterObjectEncoder extends Encoder[Http.Parameter.Object, Chain[(String, Option[String])]]:
  val dictionary = DictionaryEncoder(key = KeyPrinter, value = HttpParameterObjectValueEncoder)
  val record = RecordEncoder(field =
    FieldEncoder(key = KeyPrinter, value = HttpParameterObjectValueEncoder)
      .mapK[Http.Parameter.Field]([A] => (field: Http.Parameter.Field[A]) => field.self)
  )

  override def encode[A](schema: Http.Parameter.Object[A], a: A): Chain[(String, Option[String])] = schema match
    case Http.Parameter.Object.Dictionary(self) => Chain.fromSeq(dictionary.encode(schema = self, a))
    case Http.Parameter.Object.Record(self)     => record.encode(schema = self, a)
