package io.taig.otter.http

import cats.syntax.all.*
import io.taig.otter.Codec
import io.taig.otter.filterKeys

sealed abstract class Url[A]:
  self =>

  def path: Path[?]

  def queries: Queries[?]

  final def imap[B](f: A => B)(g: B => A): Url[B] = new Url[B]:
    export self.{path, queries}
    override def decode(values: Http.Url): Codec.Result[B] = self.decode(values).map(f)
    override def encode(b: B): Http.Url = self.encode(g(b))

  final def zip[B](url: Url[B]): Url[(A, B)] = new Url[(A, B)]:
    override def path: Path[?] = self.path.zip(url.path)
    override def queries: Queries[?] = self.queries.zip(url.queries)
    override def decode(values: Http.Url): Codec.Result[(A, B)] =
      val (pathLeft, pathRight) = values.path.splitAt(self.path.toVector.length)
      val (queriesLeft, remainders) = values.queries.filterKeys(self.queries.toVector.map(_.name))
      val (queriesRight, _) = remainders.filterKeys(url.queries.toVector.map(_.name))
      (self.decode(Http.Url(pathLeft, queriesLeft)), url.decode(Http.Url(pathRight, queriesRight))).tupled
    override def encode(ab: (A, B)): Http.Url = self.encode(ab._1) ++ url.encode(ab._2)

  def decode(values: Http.Url): Codec.Result[A]

  def encode(a: A): Http.Url

object Url:
  val Empty: Url[Unit] = new Url[Unit]:
    override def path: Path[?] = Path.Empty
    override def queries: Queries[?] = Queries.Empty
    override def decode(values: Http.Url): Codec.Result[Unit] =
      Path.Empty.decode(values.path) *> Queries.Empty.decode(values.queries)
    override def encode(a: Unit): Http.Url = Http.Url.Empty

  def apply[A, B](path: Path[A], queries: Queries[B]): Url[(A, B)] =
    val _path = path
    val _queries = queries

    new Url[(A, B)]:
      override def path: Path[A] = _path
      override def queries: Queries[B] = _queries
      override def decode(values: Http.Url): Codec.Result[(A, B)] =
        (path.decode(values.path), queries.decode(values.queries)).tupled
      override def encode(ab: (A, B)): Http.Url = Http.Url(path = path.encode(ab._1), queries = queries.encode(ab._2))

  def apply[A](path: Path[A]): Url[A] = Url(path, Queries.Empty).imap { case (a, _) => a } { case a => (a, ()) }

  def apply[A](queries: Queries[A]): Url[A] = Url(Path.Empty, queries).imap { case (_, a) => a } { case a => ((), a) }
