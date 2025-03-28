package io.taig.otter
import io.circe.Json
import io.circe.syntax.*

object CirceJsonBranchEncoder:
  def apply[A](branch: Branch[?, A], a: A): Json = ???
  // branch match
  //   case Tagged(name, codec, Discriminator.Nested(identifier, value), _) =>
  //     Json.obj(identifier := name, value := CirceJsonCodecEncoder(codec.value, a))
  //   case Tagged(name, codec, Discriminator.Merged(identifier), _) =>
  //     CirceJsonCodecEncoder(codec.value, a).deepMerge(Json.obj(identifier := name))
  //   case Tagged(name, codec, Discriminator.Keyed, _) =>
  //     Json.obj(name := CirceJsonCodecEncoder(codec.value, a))
  //   case Root(_, codec, _) => CirceJsonCodecEncoder(codec.value, a)
