package io.taig.otter

import io.circe.Json

object CirceJsonCollectionEncoder:
  def apply[A](codec: Collection[?, A], a: A): Json = codec match
    case Collection.Indexed(self, _, _, _, _) =>
      Json.fromValues(a.map(CirceJsonCodecEncoder(self.value, _)))
    case Collection.Linked(self, _, _, _, _) =>
      Json.fromValues(a.map(CirceJsonCodecEncoder(self.value, _)))
    case Collection.Modify(self, _, g) => CirceJsonCollectionEncoder(self, g(a))
