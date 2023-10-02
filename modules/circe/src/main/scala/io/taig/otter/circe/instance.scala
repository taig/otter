package io.taig.otter.circe

import io.circe.Encoder
import io.taig.otter.Data

object instance:
  given [A <: Data]: Encoder[A] = fromData(_)
  given Encoder.AsObject[Data.Object] = fromData(_)
