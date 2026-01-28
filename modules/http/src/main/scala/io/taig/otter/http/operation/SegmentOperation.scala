package io.taig.otter.http.operation

import io.taig.otter.Reference
import scala.annotation.targetName
import io.taig.otter.http.Http
import io.taig.otter.InvariantK

object SegmentOperation:
  trait Dynamic[F[_]]:
    self =>

    def lift[A](name: String, parameter: Reference[Http.Segment.Parameter, A]): F[A]

    def mapK[G[_]](fK: [A] => F[A] => G[A]): SegmentOperation.Dynamic[G] = new Dynamic[G]:
      override def lift[A](name: String, parameter: Reference[Http.Segment.Parameter, A]): G[A] =
        fK(self.lift(name, parameter))

  object Dynamic:
    trait Read[F[_]] extends SegmentOperation.Dynamic[F]:
      self =>

      @targetName("liftRead")
      def lift[A](name: String, schema: Reference[Http.Segment.Parameter.Read, A]): F[A]

      final override def lift[A](name: String, schema: Reference[Http.Segment.Parameter, A]): F[A] =
        lift(name, schema: Reference[Http.Segment.Parameter.Read, A])

      override def mapK[G[_]](fK: [A] => F[A] => G[A]): SegmentOperation.Dynamic.Read[G] = new Read[G]:
        @targetName("liftRead")
        override def lift[A](name: String, schema: Reference[Http.Segment.Parameter.Read, A]): G[A] =
          fK(self.lift(name, schema))

    object Read:
      inline def apply[F[_]](using self: SegmentOperation.Dynamic.Read[F]): SegmentOperation.Dynamic.Read[F] = self

      given InvariantK[SegmentOperation.Dynamic.Read]:
        extension [G[_]](self: SegmentOperation.Dynamic.Read[G])
          override def imapK[H[_]](fK: [A] => G[A] => H[A])(gK: [A] => H[A] => G[A]): Read[H] = self.mapK(fK)

    trait Write[F[_]] extends SegmentOperation.Dynamic[F]:
      self =>

      @targetName("liftWrite")
      def lift[A](name: String, schema: Reference[Http.Segment.Parameter.Write, A]): F[A]

      final override def lift[A](name: String, schema: Reference[Http.Segment.Parameter, A]): F[A] =
        lift(name, schema: Reference[Http.Segment.Parameter.Write, A])

      override def mapK[G[_]](fK: [A] => F[A] => G[A]): SegmentOperation.Dynamic.Write[G] = new Write[G]:
        @targetName("liftWrite")
        override def lift[A](name: String, schema: Reference[Http.Segment.Parameter.Write, A]): G[A] =
          fK(self.lift(name, schema))

    object Write:
      inline def apply[F[_]](using self: SegmentOperation.Dynamic.Write[F]): SegmentOperation.Dynamic.Write[F] = self

      given InvariantK[SegmentOperation.Dynamic.Write]:
        extension [G[_]](self: SegmentOperation.Dynamic.Write[G])
          override def imapK[H[_]](fK: [A] => G[A] => H[A])(gK: [A] => H[A] => G[A]): Write[H] = self.mapK(fK)

    inline def apply[F[_]](using self: SegmentOperation.Dynamic[F]): SegmentOperation.Dynamic[F] = self

    given InvariantK[SegmentOperation.Dynamic]:
      extension [G[_]](self: SegmentOperation.Dynamic[G])
        override def imapK[H[_]](fK: [A] => G[A] => H[A])(gK: [A] => H[A] => G[A]): Dynamic[H] = self.mapK(fK)

  trait Static[F[_]]:
    self =>

    def lift(name: String): F[Unit]

    def mapK[G[_]](fK: [A] => F[A] => G[A]): SegmentOperation.Static[G] = new Static[G]:
      override def lift(name: String): G[Unit] = fK(self.lift(name))

  object Static:
    inline def apply[F[_]](using self: SegmentOperation.Static[F]): SegmentOperation.Static[F] = self

    given InvariantK[SegmentOperation.Static]:
      extension [G[_]](self: SegmentOperation.Static[G])
        override def imapK[H[_]](fK: [A] => G[A] => H[A])(gK: [A] => H[A] => G[A]): Static[H] = self.mapK(fK)

    trait Read[F[_]] extends SegmentOperation.Static[F]:
      self =>

      override def mapK[G[_]](fK: [A] => F[A] => G[A]): SegmentOperation.Static.Read[G] = new Read[G]:
        override def lift(name: String): G[Unit] = fK(self.lift(name))

    object Read:
      inline def apply[F[_]](using self: SegmentOperation.Static.Read[F]): SegmentOperation.Static.Read[F] = self

      given InvariantK[SegmentOperation.Static.Read]:
        extension [G[_]](self: SegmentOperation.Static.Read[G])
          override def imapK[H[_]](fK: [A] => G[A] => H[A])(gK: [A] => H[A] => G[A]): Read[H] = self.mapK(fK)

    trait Write[F[_]] extends SegmentOperation.Static[F]:
      self =>

      override def mapK[G[_]](fK: [A] => F[A] => G[A]): SegmentOperation.Static.Write[G] = new Write[G]:
        override def lift(name: String): G[Unit] = fK(self.lift(name))

    object Write:
      inline def apply[F[_]](using self: SegmentOperation.Static.Write[F]): SegmentOperation.Static.Write[F] = self

      given InvariantK[SegmentOperation.Static.Write]:
        extension [G[_]](self: SegmentOperation.Static.Write[G])
          override def imapK[H[_]](fK: [A] => G[A] => H[A])(gK: [A] => H[A] => G[A]): Write[H] = self.mapK(fK)
