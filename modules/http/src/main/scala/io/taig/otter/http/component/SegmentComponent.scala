package io.taig.otter.http.component

import io.taig.otter.http.operation.SegmentOperation
import io.taig.otter.http.Http
import io.taig.otter.Reference
import io.taig.otter.component.PrimitiveComponent

object SegmentComponent:
  trait Dynamic[F[_]](using F: SegmentOperation.Dynamic[F]):
    final def apply[A](name: String, parameter: Http.Segment.Parameter[A]): F[A] =
      F.lift(name, parameter = Reference.later(parameter))

  trait Parameter
      extends PrimitiveComponent.Boolean[Http.Segment.Parameter.Primitive.Boolean],
        PrimitiveComponent.Number[Http.Segment.Parameter.Primitive.Number],
        PrimitiveComponent.Text[Http.Segment.Parameter.Primitive.Text]
