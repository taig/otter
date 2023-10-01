package io.taig.otter.openapi

import cats.data.Chain
import io.circe.{Encoder, Json, JsonObject}

opaque type Extensions = Chain[(String, Json)]

object Extensions:
  val Empty: Extensions = Chain.empty

  def apply(values: (String, Json)*): Extensions = Chain.fromSeq(values)

  given Encoder.AsObject[Extensions] = extensions =>
    JsonObject.fromFoldable(extensions.map { case (key, value) => (s"x-$key", value) })
