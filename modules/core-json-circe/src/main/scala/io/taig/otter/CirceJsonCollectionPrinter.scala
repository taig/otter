package io.taig.otter

import io.circe.Json

object CirceJsonCollectionPrinter:
  def apply[A](codec: Collection[?, A], values: A): Json = codec match
    case Collection.Indexed(self, _, _, _, _) =>
      Json.fromValues(values.map(CirceJsonCodecPrinter(self.value, _)))
    case Collection.Linked(self, _, _, _, _) =>
      Json.fromValues(values.map(CirceJsonCodecPrinter(self.value, _)))
    case Collection.Modify(self, _, g) => CirceJsonCollectionPrinter(self, g(values))
