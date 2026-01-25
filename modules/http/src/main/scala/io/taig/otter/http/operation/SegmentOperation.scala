package io.taig.otter.http.operation

import io.taig.otter.http.Segment
import io.taig.otter.Reference
import scala.annotation.targetName

object SegmentOperation:
  trait Parameter[F[_]]:
    def lift[A](name: String, schema: Reference[Segment.Parameter.Value, A]): F[A]

  object Parameter:
    trait Read[F[_]] extends SegmentOperation.Parameter[F]:
      @targetName("liftRead")
      def lift[A](name: String, schema: Reference[Segment.Parameter.Value.Read, A]): F[A]

      final override def lift[A](name: String, schema: Reference[Segment.Parameter.Value, A]): F[A] =
        lift(name, schema: Reference[Segment.Parameter.Value.Read, A])

    trait Write[F[_]] extends SegmentOperation.Parameter[F]:
      @targetName("liftRead")
      def lift[A](name: String, schema: Reference[Segment.Parameter.Value.Write, A]): F[A]

  trait Static[F[_]]:
    def lift(name: String): F[Unit]
