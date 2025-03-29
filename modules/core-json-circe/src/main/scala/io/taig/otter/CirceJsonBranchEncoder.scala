package io.taig.otter

import io.circe.Json
import io.circe.syntax.*

object CirceJsonBranchEncoder:
  def apply[A](branch: Branch[?, A], a: A, discriminator: Option[Discriminator]): Json = discriminator match
    case Some(Discriminator.Nested(identifier, value)) =>
      Json.obj(identifier := branch.name, value := CirceJsonCodecEncoder(codec = branch.codec.value, a))
    case Some(Discriminator.Merged(identifier)) =>
      CirceJsonCodecEncoder(codec = branch.codec.value, a).deepMerge(Json.obj(identifier := branch.name))
    case Some(Discriminator.Keyed) =>
      Json.obj(branch.name := CirceJsonCodecEncoder(codec = branch.codec.value, a))
    case None => CirceJsonCodecEncoder(codec = branch.codec.value, a)
