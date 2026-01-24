package io.taig.otter.http

import cats.Contravariant
import cats.Functor
import cats.Invariant
import io.taig.otter.Reference
import io.taig.otter.Value

type Segment[A] = Segment.Read[A] & Segment.Write[A]

object Segment:
  sealed trait Read[+A]

  sealed trait Write[-A]

  type Parameter[A] = Segment.Parameter.Read[A] & Segment.Parameter.Write[A]

  object Parameter:
    sealed trait Read[+A] extends Segment.Read[A]:
      final def map[B](f: A => B): Segment.Parameter.Read[B] = Read.Modify(self = this, f)

      def name: String

    object Read:
      final case class Modify[A, B](self: Segment.Parameter.Read[A], f: A => B) extends Segment.Parameter.Read[B]:
        export self.name

      final case class Root[A](name: String, schema: Reference[Value.Read, A]) extends Segment.Parameter.Read[A]

      given Functor[Segment.Parameter.Read]:
        override def map[A, B](segment: Segment.Parameter.Read[A])(f: A => B): Segment.Parameter.Read[B] =
          segment.map(f)

    sealed trait Write[-A] extends Segment.Write[A]:
      final def contramap[B](f: B => A): Segment.Parameter.Write[B] = Write.Modify(self = this, f)

      def name: String

    object Write:
      final case class Modify[A, B](self: Segment.Parameter.Write[A], f: B => A) extends Segment.Parameter.Write[B]:
        export self.name

      final case class Root[A](name: String, schema: Reference[Value.Write, A]) extends Segment.Parameter.Write[A]

      given Contravariant[Segment.Parameter.Write]:
        override def contramap[A, B](segment: Segment.Parameter.Write[A])(f: B => A): Segment.Parameter.Write[B] =
          segment.contramap(f)

    final case class Modify[A, B](self: Segment.Parameter[A], f: A => B, g: B => A)
        extends Segment.Parameter.Read[B],
          Segment.Parameter.Write[B]:
      export self.name

    final case class Root[A](name: String, schema: Reference[Value, A])
        extends Segment.Parameter.Read[A],
          Segment.Parameter.Write[A]

    given Invariant[Segment.Parameter]:
      override def imap[A, B](self: Segment.Parameter[A])(f: A => B)(g: B => A): Segment.Parameter[B] =
        Modify(self, f, g)

  type Static[A] = Segment.Static.Read[A] & Segment.Static.Write[A]

  object Static:
    sealed trait Read[+A] extends Segment.Read[A]:
      final def map[B](f: A => B): Segment.Static.Read[B] = Read.Modify(self = this, f)

    object Read:
      final case class Modify[A, B](self: Segment.Static.Read[A], f: A => B) extends Segment.Static.Read[B]

      given Functor[Segment.Static.Read]:
        override def map[A, B](segment: Segment.Static.Read[A])(f: A => B): Segment.Static.Read[B] =
          segment.map(f)

    sealed trait Write[-A] extends Segment.Write[A]:
      final def contramap[B](f: B => A): Segment.Static.Write[B] = Write.Modify(self = this, f)

    object Write:
      final case class Modify[A, B](self: Segment.Static.Write[A], f: B => A) extends Segment.Static.Write[B]

      given Contravariant[Segment.Static.Write]:
        override def contramap[A, B](segment: Segment.Static.Write[A])(f: B => A): Segment.Static.Write[B] =
          segment.contramap(f)

    final case class Modify[A, B](self: Segment.Static[A], f: A => B, g: B => A)
        extends Segment.Static.Read[B],
          Segment.Static.Write[B]

    final case class Root(name: String) extends Segment.Static.Read[Unit], Segment.Static.Write[Unit]

    given Invariant[Segment.Static]:
      override def imap[A, B](self: Segment.Static[A])(f: A => B)(g: B => A): Segment.Static[B] = Modify(self, f, g)
