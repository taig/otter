package io.taig.otter.http

import io.taig.otter as Self
import cats.Functor
import cats.Contravariant
import cats.Invariant
import io.taig.otter.Reference
import Self.http.operation.SegmentOperation

sealed abstract class Segment[+F[_], A] extends Segment.Read[F, A], Segment.Write[F, A]:
  def imap[B](f: A => B)(g: B => A): Segment[F, B]

object Segment:
  sealed trait Read[+F[_], +A]:
    def map[B](f: A => B): Segment.Read[F, B]

    def name: String

  object Read:
    given [F[_]] => Functor[Segment.Read[F, *]]:
      override def map[A, B](segment: Segment.Read[F, A])(f: A => B): Segment.Read[F, B] = segment.map(f)

  sealed trait Write[+F[_], -A]:
    def contramap[B](f: B => A): Segment.Write[F, B]

    def name: String

  object Write:
    given [F[_]] => Contravariant[Segment.Write[F, *]]:
      override def contramap[A, B](segment: Segment.Write[F, A])(f: B => A): Segment.Write[F, B] = segment.contramap(f)

  sealed abstract class Dynamic[+F[_], A]
      extends Segment[F, A],
        Segment.Dynamic.Read[F, A],
        Segment.Dynamic.Write[F, A]:
    final override def imap[B](f: A => B)(g: B => A): Segment.Dynamic[F, B] = Dynamic.Modify(self = this, f, g)

    override def parameter: Reference[F, ?]

  object Dynamic:
    sealed trait Read[+F[_], +A] extends Segment.Read[F, A]:
      final def map[B](f: A => B): Segment.Dynamic.Read[F, B] = Read.Modify(self = this, f)

      def name: String

      def parameter: Reference[F, ?]

    object Read:
      final case class Modify[F[_], A, B](self: Segment.Dynamic.Read[F, A], f: A => B)
          extends Segment.Dynamic.Read[F, B]:
        export self.{name, parameter}

      given [F[_]] => Functor[Segment.Dynamic.Read[F, *]]:
        override def map[A, B](segment: Segment.Dynamic.Read[F, A])(f: A => B): Segment.Dynamic.Read[F, B] =
          segment.map(f)

      given [F[_]] => SegmentOperation.Dynamic.Read[Segment.Dynamic.Read[F, *], F]:
        override def lift[A](name: String, schema: Reference[F, A]): Segment.Dynamic.Read[F, A] =
          Root(name, schema)

    sealed trait Write[+F[_], -A] extends Segment.Write[F, A]:
      final override def contramap[B](f: B => A): Segment.Dynamic.Write[F, B] = Write.Modify(self = this, f)

      def parameter: Reference[F, ?]

    object Write:
      final case class Modify[F[_], A, B](self: Segment.Dynamic.Write[F, A], f: B => A)
          extends Segment.Dynamic.Write[F, B]:
        export self.{name, parameter}

      given [F[_]] => Contravariant[Segment.Dynamic.Write[F, *]]:
        override def contramap[A, B](segment: Segment.Dynamic.Write[F, A])(f: B => A): Segment.Dynamic.Write[F, B] =
          segment.contramap(f)

      given [F[_]] => SegmentOperation.Dynamic.Write[Segment.Dynamic.Write[F, *], F]:
        override def lift[A](
            name: String,
            schema: Reference[F, A]
        ): Segment.Dynamic.Write[F, A] = Root(name, schema)

    final case class Modify[F[_], A, B](self: Segment.Dynamic[F, A], f: A => B, g: B => A)
        extends Segment.Dynamic[F, B]:
      export self.{name, parameter}

    final case class Root[F[_], A](name: String, parameter: Reference[F, A]) extends Segment.Dynamic[F, A]

    given [F[_]] => Invariant[Segment.Dynamic[F, *]]:
      override def imap[A, B](self: Segment.Dynamic[F, A])(f: A => B)(g: B => A): Segment.Dynamic[F, B] =
        Modify(self, f, g)

    given [F[_]] => SegmentOperation.Dynamic[Segment.Dynamic[F, *], F]:
      override def lift[A](name: String, schema: Reference[F, A]): Segment.Dynamic[F, A] =
        Root(name, schema)

  sealed abstract class Static[A] extends Segment[Nothing, A], Segment.Static.Read[A], Segment.Static.Write[A]:
    override def imap[B](f: A => B)(g: B => A): Segment.Static[B] = Segment.Static.Modify(self = this, f, g)

  object Static:
    sealed trait Read[+A] extends Segment.Read[Nothing, A]:
      final def map[B](f: A => B): Segment.Static.Read[B] = Read.Modify(self = this, f)

    object Read:
      final case class Modify[A, B](self: Segment.Static.Read[A], f: A => B) extends Segment.Static.Read[B]:
        export self.name

      given Functor[Segment.Static.Read]:
        override def map[A, B](segment: Segment.Static.Read[A])(f: A => B): Segment.Static.Read[B] =
          segment.map(f)

      given SegmentOperation.Static.Read[Segment.Static.Read]:
        override def lift(name: String): Segment.Static.Read[Unit] = Root(name)

    sealed trait Write[-A] extends Segment.Write[Nothing, A]:
      final override def contramap[B](f: B => A): Segment.Static.Write[B] = Write.Modify(self = this, f)

    object Write:
      final case class Modify[A, B](self: Segment.Static.Write[A], f: B => A) extends Segment.Static.Write[B]:
        export self.name

      given Contravariant[Segment.Static.Write]:
        override def contramap[A, B](segment: Segment.Static.Write[A])(f: B => A): Segment.Static.Write[B] =
          segment.contramap(f)

      given SegmentOperation.Static.Write[Segment.Static.Write]:
        override def lift(name: String): Segment.Static.Write[Unit] = Root(name)

    final case class Modify[A, B](self: Segment.Static[A], f: A => B, g: B => A) extends Segment.Static[B]:
      export self.name

    final case class Root(name: String) extends Segment.Static[Unit]

    given Invariant[Segment.Static]:
      override def imap[A, B](self: Segment.Static[A])(f: A => B)(g: B => A): Segment.Static[B] = Modify(self, f, g)

    given SegmentOperation.Static[Segment.Static]:
      override def lift(name: String): Segment.Static[Unit] = Root(name)

  given [F[_]] => Invariant[Segment[F, *]]:
    override def imap[A, B](self: Segment[F, A])(f: A => B)(g: B => A): Segment[F, B] = self match
      case segment: Segment.Dynamic[F, A] => segment.imap(f)(g)
      case segment: Segment.Static[A]     => segment.imap(f)(g)
