package io.taig.otter.http

import cats.InvariantSemigroupal
import cats.data.{Chain, Validated}
import cats.syntax.all.*
import io.taig.otter.Evidence
import io.taig.otter.validation.Violations

sealed abstract class Url[A]:
  self =>
  def path: Path[?]
  def queries: Queries[?]

  final def imap[B](f: A => B)(g: B => A): Url[B] = new Url[B]:
    export self.{path, queries}
    override def decodeWithRemainders(remainders: Http.Url): Validated[Violations, (Http.Url, B)] =
      self.decodeWithRemainders(remainders).map(_.map(f))
    override def encode(b: B): Http.Url = self.encode(g(b))

  final infix def product[B](url: Url[B]): Url[(A, B)] = new Url[(A, B)]:
    override def path: Path[?] = self.path.zip(url.path)
    override def queries: Queries[?] = self.queries.zip(url.queries)
    override def decodeWithRemainders(remainders: Http.Url): Validated[Violations, (Http.Url, (A, B))] = self
      .decodeWithRemainders(remainders)
      .andThen { case (remainders, a) => url.decodeWithRemainders(remainders).map(_.tupleLeft(a)) }
    override def encode(ab: (A, B)): Http.Url = self.encode(ab._1) ++ url.encode(ab._2)

  final infix def zip[B](url: Url[B])(using evidence: Evidence.Merge[A, B]): Url[evidence.Out] =
    product(url).imap(evidence.apply)(evidence.unapply)
  final def /[B](path: Path[B])(using evidence: Evidence.Merge[A, B]): Url[evidence.Out] = zip(path.toUrl)
  final def /[B](segment: Segment[B])(using evidence: Evidence.Merge[A, B]): Url[evidence.Out] = /(segment.toPath)
  final def /(static: String): Url[A] = /(Segment.Static(static))
  final def +?[B](queries: Queries[B])(using evidence: Evidence.Merge[A, B]): Url[evidence.Out] = zip(queries.toUrl)
  final def +?[B](query: Query[B])(using evidence: Evidence.Merge[A, B]): Url[evidence.Out] = +?(query.toQueries)

  final def matches(url: Http.Url): Boolean = path.matches(url.path) && queries.matches(url.queries)

  final def decode(url: Http.Url): Validated[Violations, A] = decodeWithRemainders(url).map(_._2)
  def decodeWithRemainders(remainders: Http.Url): Validated[Violations, (Http.Url, A)]
  def encode(a: A): Http.Url

  final def print: String = path.print

object Url:
  val Root: Url[Unit] = new Url[Unit]:
    override def path: Path[?] = Path.Root
    override def queries: Queries[?] = Queries.Empty
    override def decodeWithRemainders(remainders: Http.Url): Validated[Violations, (Http.Url, Unit)] =
      (remainders, ()).valid
    override def encode(a: Unit): Http.Url = Http.Url.Empty

  def apply[A](of: Path[A]): Url[A] = new Url[A]:
    override def path: Path[A] = of
    override def queries: Queries[Unit] = Queries.Empty
    override def decodeWithRemainders(remainders: Http.Url): Validated[Violations, (Http.Url, A)] =
      path.decodeWithRemainders(remainders._1).map { case (path, a) => (Http.Url(path, remainders.queries), a) }
    override def encode(a: A): Http.Url = Http.Url(path.encode(a), Chain.empty)

  def apply[A](of: Queries[A]): Url[A] = new Url[A]:
    override def path: Path[Unit] = Path.Root
    override def queries: Queries[A] = of
    override def decodeWithRemainders(remainders: Http.Url): Validated[Violations, (Http.Url, A)] =
      queries.decodeWithRemainders(remainders._2).map { case (queries, a) => (Http.Url(remainders.path, queries), a) }
    override def encode(a: A): Http.Url = Http.Url(Chain.empty, queries.encode(a))

  given InvariantSemigroupal[Url] with
    override def imap[A, B](fa: Url[A])(f: A => B)(g: B => A): Url[B] = fa.imap(f)(g)
    override def product[A, B](fa: Url[A], fb: Url[B]): Url[(A, B)] = fa.product(fb)
