package io.taig.otter.http

import io.taig.otter.Reference
import scala.annotation.targetName

trait HttpDsl:
  object segment:
    val string = Segment.Codec.Primitive.invariant.string

    def apply(name: String): Segment.Static = Segment.Static(name)
    def apply[A](name: String, codec: => Segment.Codec[A]): Segment.Parameter[A] =
      Segment.Parameter.Value(name, codec = Reference.later(codec), style = Segment.Style.Simple)

    @targetName("array")
    def apply[A](name: String, codec: => Segment.Codec.Array[A]): Segment.Parameter[A] =
      Segment.Parameter.Array(name, codec = Reference.later(codec), explode = false, style = Segment.Style.Simple)

    @targetName("obj")
    def apply[A](name: String, codec: => Segment.Codec.Object[A]): Segment.Parameter[A] =
      Segment.Parameter.Object(name, codec = Reference.later(codec), explode = false, style = Segment.Style.Simple)

  object path:
    val empty: Path[Unit] = Path.Empty

object HttpDsl extends HttpDsl
