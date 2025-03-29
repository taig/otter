package io.taig.otter

import io.circe.Json
import cats.syntax.all.*

object CirceJsonUnionEncoder:
  def apply[A](codec: Union[?, ?, A], a: A): Json = codec match
    case codec: Union.Untagged[?, A] => CirceJsonUnionEncoder(codec, a, discriminator = none)
    case Union.Tagged.Keyed(untagged) =>
      CirceJsonUnionEncoder(codec = untagged, a, discriminator = Discriminator.Keyed.some)
    case Union.Tagged.Merged(untagged, discriminator) =>
      CirceJsonUnionEncoder(codec = untagged, a, discriminator = discriminator.some)
    case Union.Tagged.Nested(untagged, discriminator) =>
      CirceJsonUnionEncoder(codec = untagged, a, discriminator = discriminator.some)

  def apply[A](codec: Union.Untagged[?, A], a: A, discriminator: Option[Discriminator]): Json = codec match
    case Union.Untagged.Modify(self, _, g) => CirceJsonUnionEncoder(self, g(a))
    case Union.Untagged.Root(branch, _)    => CirceJsonBranchEncoder(branch, a, discriminator)
    case Union.Untagged.OrElse(left, right, _) =>
      a.fold(CirceJsonCodecEncoder(left, _), CirceJsonCodecEncoder(right, _))
