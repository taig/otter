package io.taig.otter
import io.circe.Json
import io.circe.syntax.*
import io.taig.otter.Branch.Root
import io.taig.otter.Branch.Tagged

object CirceJsonBranchPrinter:
  def apply[A](branch: Branch[?, A], a: A): Json = branch match
    case Tagged(name, codec, Discriminator.Nested(identifier, value), _) =>
      Json.obj(identifier := name, value := CirceJsonCodecPrinter(codec.value, a))
    case Tagged(name, codec, Discriminator.Merged(identifier), _) =>
      CirceJsonCodecPrinter(codec.value, a).deepMerge(Json.obj(identifier := name))
    case Tagged(name, codec, Discriminator.Keyed, _) =>
      Json.obj(name := CirceJsonCodecPrinter(codec.value, a))
    case Root(_, codec, _) => CirceJsonCodecPrinter(codec.value, a)
