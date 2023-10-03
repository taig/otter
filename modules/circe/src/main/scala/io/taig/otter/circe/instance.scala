package io.taig.otter.circe

import cats.data.Chain
import io.circe.{Encoder, JsonObject, KeyEncoder}
import io.circe.syntax.*
import io.taig.otter.Data
import org.typelevel.ci.CIString

object instance:
  given [A <: Data]: Encoder[A] = fromData(_)
  given Encoder.AsObject[Data.Object] = fromData(_)

  given KeyEncoder[CIString] = _.toString

  given [A, B: Encoder](using a: KeyEncoder[A]): Encoder.AsObject[Chain[(A, B)]] = values =>
    JsonObject.fromFoldable(values.map { case (key, value) => (a(key), value.asJson) })
