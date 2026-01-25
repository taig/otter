package io.taig.otter.http

import cats.data.Chain
import io.taig.otter.Reference
import cats.Apply
import cats.ContravariantSemigroupal
import cats.InvariantSemigroupal
import io.taig.otter.http.operation.PathOperation

sealed abstract class Path[+F[_], A] extends Path.Read[F, A], Path.Write[F, A]

object Path:
  sealed trait Read[+F[_], +A]:
    def segments: Chain[Reference[F, ?]]

  object Read:
    final case class Modify[F[_], A, B](self: Path.Read[F, A], f: A => B) extends Path.Read[F, B]:
      export self.segments

    final case class Product[F[_], A, B](left: Path.Read[F, A], right: Path.Read[F, B]) extends Path.Read[F, (A, B)]:
      override def segments: Chain[Reference[F, ?]] = left.segments ++ right.segments

    given [F[_]] => Apply[Path.Read[F, *]]:
      override def ap[A, B](ff: Path.Read[F, A => B])(fa: Path.Read[F, A]): Path.Read[F, B] =
        map(Product(ff, fa))(_ apply _)

      override def map[A, B](self: Path.Read[F, A])(f: A => B): Path.Read[F, B] = Modify(self, f)

  sealed trait Write[+F[_], -A]:
    def segments: Chain[Reference[F, ?]]

  object Write:
    final case class Modify[F[_], A, B](self: Path.Write[F, A], f: B => A) extends Path.Write[F, B]:
      export self.segments

    final case class Product[F[_], A, B](left: Path.Write[F, A], right: Path.Write[F, B]) extends Path.Write[F, (A, B)]:
      override def segments: Chain[Reference[F, ?]] = left.segments ++ right.segments

    given [F[_]] => ContravariantSemigroupal[Path.Write[F, *]]:
      override def product[A, B](fa: Path.Write[F, A], fb: Path.Write[F, B]): Path.Write[F, (A, B)] =
        Product(fa, fb)

      override def contramap[A, B](self: Path.Write[F, A])(f: B => A): Path.Write[F, B] = Modify(self, f)

  case object Empty extends Path[Nothing, Unit]:
    override def segments: Chain[Nothing] = Chain.empty

  final case class Modify[F[_], A, B](self: Path[F, A], f: A => B, g: B => A) extends Path[F, B]:
    override def segments: Chain[Reference[F, ?]] = self.segments

  final case class Product[F[_], A, B](left: Path[F, A], right: Path[F, B]) extends Path[F, (A, B)]:
    override def segments: Chain[Reference[F, ?]] = left.segments ++ right.segments

  final case class Root[F[_], A](segment: Reference[F, A]) extends Path[F, A]:
    override def segments: Chain[Reference[F, ?]] = Chain.one(segment)

  given [F[_]] => InvariantSemigroupal[Path[F, *]]:
    override def imap[A, B](self: Path[F, A])(f: A => B)(g: B => A): Path[F, B] = Modify(self, f, g)
    override def product[A, B](fa: Path[F, A], fb: Path[F, B]): Path[F, (A, B)] = Product(fa, fb)

  given [F[_]] => PathOperation[Path[F, *]] = ???
