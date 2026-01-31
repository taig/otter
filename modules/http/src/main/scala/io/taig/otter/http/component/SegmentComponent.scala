package io.taig.otter.http.component

import io.taig.otter.http.Segment
import io.taig.otter.http.operation.SegmentOperation
import io.taig.otter.Reference
import io.taig.otter.component.PrimitiveComponent

object SegmentComponent:
  trait Dynamic[F[_], G[_]](using F: SegmentOperation.Dynamic[F, G]):
    final def apply[A](name: String, parameter: => G[A]): F[A] =
      F.lift(name, parameter = Reference.later(parameter))

  trait Parameter
      extends PrimitiveComponent.Boolean[Segment.Parameter.Primitive.Boolean],
        PrimitiveComponent.Number[Segment.Parameter.Primitive.Number],
        PrimitiveComponent.Text[Segment.Parameter.Primitive.Text]
