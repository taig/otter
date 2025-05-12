package io.taig.otter.http

import io.taig.otter.Metadata
import io.taig.otter.Reference

trait SegmentDsl:
  def parameter[A](name: String, codec: => Http.Parameter[A]): Segment[A] =
    Segment.Parameter(name, codec = Reference.later(codec), metadata = Metadata.Empty)

object SegmentDsl extends SegmentDsl
