package io.taig.otter

import io.circe.Json

object CirceJsonOptionalEncoder:
  def apply[A](codec: Optional[?, A], a: A): Json = codec match
    case Optional.Default(codec, _, _) => CirceJsonCodecEncoder(codec, a)
    case Optional.Modify(self, _, g)   => CirceJsonOptionalEncoder(self, g(a))
    case Optional.Null(_)              => Json.Null
    case Optional.Nullable(codec, _)   => a.fold(Json.Null)(CirceJsonCodecEncoder(codec, _))
    case Optional.Void(_)              => Json.Null
