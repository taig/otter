package io.taig.otter.http.operation

import io.taig.otter.Reference
import io.taig.otter.InvariantK

object SegmentOperation:
  trait Dynamic[F[_], G[_]]:
    self =>

    def lift[A](name: String, parameter: Reference[G, A]): F[A]

    def mapK[H[_]](fK: [A] => F[A] => H[A]): SegmentOperation.Dynamic[H, G] = new Dynamic[H, G]:
      override def lift[A](name: String, parameter: Reference[G, A]): H[A] =
        fK(self.lift(name, parameter))

  object Dynamic:
    trait Read[F[_], G[_]] extends SegmentOperation.Dynamic[F, G]:
      self =>

      override def mapK[H[_]](fK: [A] => F[A] => H[A]): SegmentOperation.Dynamic.Read[H, G] = new Read[H, G]:
        override def lift[A](name: String, schema: Reference[G, A]): H[A] = fK(self.lift(name, schema))

    object Read:
      inline def apply[F[_], G[_]](using
          self: SegmentOperation.Dynamic.Read[F, G]
      ): SegmentOperation.Dynamic.Read[F, G] = self

      given [F[_]] => InvariantK[[f[_]] =>> SegmentOperation.Dynamic.Read[f, F]]:
        extension [G[_]](self: SegmentOperation.Dynamic.Read[G, F])
          override def imapK[H[_]](fK: [A] => G[A] => H[A])(
              gK: [A] => H[A] => G[A]
          ): SegmentOperation.Dynamic.Read[H, F] =
            self.mapK(fK)

    trait Write[F[_], G[_]] extends SegmentOperation.Dynamic[F, G]:
      self =>

      override def mapK[H[_]](fK: [A] => F[A] => H[A]): SegmentOperation.Dynamic.Write[H, G] = new Write[H, G]:
        override def lift[A](name: String, schema: Reference[G, A]): H[A] = fK(self.lift(name, schema))

    object Write:
      inline def apply[F[_], G[_]](using
          self: SegmentOperation.Dynamic.Write[F, G]
      ): SegmentOperation.Dynamic.Write[F, G] = self

      given [F[_]] => InvariantK[[f[_]] =>> SegmentOperation.Dynamic.Write[f, F]]:
        extension [G[_]](self: SegmentOperation.Dynamic.Write[G, F])
          override def imapK[H[_]](fK: [A] => G[A] => H[A])(gK: [A] => H[A] => G[A]): Write[H, F] = self.mapK(fK)

    inline def apply[F[_], G[_]](using self: SegmentOperation.Dynamic[F, G]): SegmentOperation.Dynamic[F, G] = self

    given [F[_]] => InvariantK[[f[_]] =>> SegmentOperation.Dynamic[f, F]]:
      extension [G[_]](self: SegmentOperation.Dynamic[G, F])
        override def imapK[H[_]](fK: [A] => G[A] => H[A])(gK: [A] => H[A] => G[A]): SegmentOperation.Dynamic[H, F] =
          self.mapK(fK)

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
