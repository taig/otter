package io.taig.otter.http

import cats.InvariantSemigroupal
import cats.data.{Chain, Validated}
import cats.syntax.all.*
import io.taig.otter.schema.Violations

sealed abstract class Url[A]:
  self =>
  def path: Path[?]
  def queries: Queries[?]

  final def imap[B](f: A => B)(g: B => A): Url[B] = new Url[B]:
    export self.{path, queries}
    override def decodeWithRemainders(
        remainders: (Http.Path, Http.Queries)
    ): Validated[Violations, ((Http.Path, Http.Queries), B)] =
      self.decodeWithRemainders(remainders).map(_.map(f))
    override def encode(b: B): (Http.Path, Http.Queries) = self.encode(g(b))

  final def zip[B](url: Url[B]): Url[(A, B)] = new Url[(A, B)]:
    override def path: Path[?] = self.path.zip(url.path)
    override def queries: Queries[?] = self.queries.zip(url.queries)
    override def decodeWithRemainders(
        remainders: (Http.Path, Http.Queries)
    ): Validated[Violations, ((Http.Path, Http.Queries), (A, B))] = self
      .decodeWithRemainders(remainders)
      .andThen { case (remainders, a) => url.decodeWithRemainders(remainders).map(_.tupleLeft(a)) }
    override def encode(ab: (A, B)): (Http.Path, Http.Queries) =
      val (path1, queries1) = self.encode(ab._1)
      val (path2, queries2) = url.encode(ab._2)
      (path1 ++ path2, queries1 ++ queries2)

  final def decode(path: Http.Path, queries: Http.Queries): Validated[Violations, A] =
    decodeWithRemainders(path, queries).map(_._2)
  def decodeWithRemainders(remainders: (Http.Path, Http.Queries)): Validated[Violations, ((Http.Path, Http.Queries), A)]
  def encode(a: A): (Http.Path, Http.Queries)

object Url:
  val Root: Url[Unit] = new Url[Unit]:
    override def path: Path[?] = Path.Root
    override def queries: Queries[?] = Queries.Empty
    override def decodeWithRemainders(
        remainders: (Http.Path, Http.Queries)
    ): Validated[Violations, ((Http.Path, Http.Queries), Unit)] =
      (remainders, ()).valid
    override def encode(a: Unit): (Http.Path, Http.Queries) = (Chain.empty, Chain.empty)

  def apply[A](of: Path[A]): Url[A] = new Url[A]:
    override def path: Path[A] = of
    override def queries: Queries[Unit] = Queries.Empty
    override def decodeWithRemainders(
        remainders: (Http.Path, Http.Queries)
    ): Validated[Violations, ((Http.Path, Http.Queries), A)] =
      path.decodeWithRemainders(remainders._1).map(_.leftMap((_, remainders._2)))
    override def encode(a: A): (Http.Path, Http.Queries) = (path.encode(a), Chain.empty)

  def apply[A](of: Queries[A]): Url[A] = new Url[A]:
    override def path: Path[Unit] = Path.Root
    override def queries: Queries[A] = of
    override def decodeWithRemainders(
        remainders: (Http.Path, Http.Queries)
    ): Validated[Violations, ((Http.Path, Http.Queries), A)] =
      queries.decodeWithRemainders(remainders._2).map(_.leftMap((remainders._1, _)))
    override def encode(a: A): (Http.Path, Http.Queries) = (Chain.empty, queries.encode(a))

  given InvariantSemigroupal[Url] with
    override def imap[A, B](fa: Url[A])(f: A => B)(g: B => A): Url[B] = fa.imap(f)(g)
    override def product[A, B](fa: Url[A], fb: Url[B]): Url[(A, B)] = fa.product(fb)
