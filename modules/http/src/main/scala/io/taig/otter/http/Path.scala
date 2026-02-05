package io.taig.otter.http

import cats.data.Chain
import io.taig.otter.Reference
import cats.Apply
import cats.ContravariantSemigroupal
import cats.InvariantSemigroupal
import io.taig.otter.http.operation.PathOperation
import io.taig.otter.Annotation

type Path[A] = Annotation[Path.Schema[A]]

object Path:
  type Read[A] = Annotation[Path.Schema.Read[A]]

  type Write[A] = Annotation[Path.Schema.Write[A]]

  sealed abstract class Schema[A] extends Schema.Read[A], Schema.Write[A]:
    final def product[B](path: Schema[B]): Schema[(A, B)] = Schema.Product(left = this, right = path)

    override def segments: Chain[Reference[Segment, ?]]

  object Schema:
    sealed trait Read[+A]:
      final def product[B](path: Schema.Read[B]): Schema.Read[(A, B)] =
        Schema.Read.Product(left = this, right = path)

      def segments: Chain[Reference[Segment.Read, ?]]

    object Read:
      final case class Modify[A, B](self: Schema.Read[A], f: A => B) extends Schema.Read[B]:
        export self.segments

      final case class Product[A, B](left: Schema.Read[A], right: Schema.Read[B]) extends Schema.Read[(A, B)]:
        override def segments: Chain[Reference[Segment.Read, ?]] = left.segments ++ right.segments

      given Apply[Schema.Read]:
        override def ap[A, B](ff: Schema.Read[A => B])(fa: Schema.Read[A]): Schema.Read[B] =
          map(Product(ff, fa))(_ apply _)

        override def map[A, B](self: Schema.Read[A])(f: A => B): Schema.Read[B] = Modify(self, f)

    sealed trait Write[-A]:
      final def product[B](path: Schema.Write[B]): Schema.Write[(A, B)] =
        Schema.Write.Product(left = this, right = path)

      def segments: Chain[Reference[Segment.Write, ?]]

    object Write:
      final case class Modify[A, B](self: Schema.Write[A], f: B => A) extends Schema.Write[B]:
        export self.segments

      final case class Product[A, B](left: Schema.Write[A], right: Schema.Write[B]) extends Schema.Write[(A, B)]:
        override def segments: Chain[Reference[Segment.Write, ?]] = left.segments ++ right.segments

      given ContravariantSemigroupal[Schema.Write]:
        override def product[A, B](fa: Schema.Write[A], fb: Schema.Write[B]): Schema.Write[(A, B)] = Product(fa, fb)

        override def contramap[A, B](self: Schema.Write[A])(f: B => A): Schema.Write[B] = Modify(self, f)

    case object Empty extends Schema[Unit]:
      override def segments: Chain[Nothing] = Chain.empty

    final case class Modify[A, B](self: Schema[A], f: A => B, g: B => A) extends Schema[B]:
      override def segments: Chain[Reference[Segment, ?]] = self.segments

    final case class Product[A, B](left: Schema[A], right: Schema[B]) extends Schema[(A, B)]:
      override def segments: Chain[Reference[Segment, ?]] = left.segments ++ right.segments

    final case class Root[A](segment: Reference[Segment, A]) extends Schema[A]:
      override def segments: Chain[Reference[Segment, ?]] = Chain.one(segment)

    given InvariantSemigroupal[Path.Schema]:
      override def imap[A, B](self: Path.Schema[A])(f: A => B)(g: B => A): Path.Schema[B] = Modify(self, f, g)

      override def product[A, B](fa: Path.Schema[A], fb: Path.Schema[B]): Path.Schema[(A, B)] = Product(fa, fb)

    given PathOperation[Path, Segment] = ???
