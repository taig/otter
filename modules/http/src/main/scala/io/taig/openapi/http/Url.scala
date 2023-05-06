package io.taig.openapi.http

import cats.data.{Chain, Validated}
import cats.syntax.all.*
import io.taig.openapi.History
import io.taig.openapi.schema.{Violations, Void}

import scala.collection.immutable.VectorMap

sealed abstract class Url[A]:
  def path: Path[?]
  def queries: Queries[?]
  final def matches(path: Chain[String], queries: VectorMap[String, String]): Boolean =
    val (remainders, _, result) = matchesWithRemainders(path, queries)
    result && remainders.isEmpty
  def matchesWithRemainders(
      path: Chain[String],
      queries: VectorMap[String, String]
  ): (Chain[String], VectorMap[String, String], Boolean)
  final def product[B](url: Url[B]): Url[(A, B)] = Url.Product(this, url)
  final def product[B](path: Path[B]): Url[(A, B)] = product(path.toUrl)
  final def product[B](queries: Queries[B]): Url[(A, B)] = product(queries.toUrl)
  final transparent inline def zip[B](url: Url[B]): Url[?] = inline (this, url) match
    case (left: Url[Void], right) => left.product(right).imap[B] { case (_, b) => b }(b => (Void, b))
    case (left, right: Url[Void]) => left.product(right).imap[A] { case (a, _) => a }(a => (a, Void))
    case (left: Url[? *: ?], right) =>
      left.product(right).imap { case (a, b) => a :* b }(ab => (ab.init.asInstanceOf[A], ab.last.asInstanceOf[B]))
    case (left, right) => left.product(right)
  final transparent inline def zip[B](path: Path[B]): Url[?] = zip(path.toUrl)
  final transparent inline def zip[B](queries: Queries[B]): Url[?] = zip(queries.toUrl)
  final transparent inline def /[B](segment: Segment[B]): Url[?] = zip(segment.toPath)
  final transparent inline def /(name: String): Url[?] = /(Segment.Static(name))
  final transparent inline def &[B](query: Query[B]): Url[?] = zip(query.toQueries)
  final def imap[B](f: A => B)(g: B => A): Url[B] = Url.Modify(this, f, g)
  final def decode(
      path: Chain[String],
      queries: VectorMap[String, String]
  ): Validated[Violations, A] = decodeWithRemainders(path, queries).map(_._3)
  def decodeWithRemainders(
      path: Chain[String],
      queries: VectorMap[String, String]
  ): Validated[Violations, (Chain[String], VectorMap[String, String], A)]
  def encode(a: A): (Chain[String], VectorMap[String, String])

object Url:
  private def renderPath(path: Chain[String]): String =
    "/" + path.mkString_("/")
  private def renderQueries(queries: VectorMap[String, String]): String =
    queries.toSeq.map { case (name, value) => s"$name=$value" }.mkString_("&")
  private def renderUrl(path: Chain[String], queries: VectorMap[String, String]): String =
    if queries.isEmpty then renderPath(path) else renderPath(path) + "?" + renderQueries(queries)

  final private case class FromPath[A](path: Path[A]) extends Url[A]:
    override def queries: Queries[?] = Queries.Empty
    override def matchesWithRemainders(
        path: Chain[String],
        queries: VectorMap[String, String]
    ): (Chain[String], VectorMap[String, String], Boolean) =
      val (remainders, result) = this.path.matchesWithRemainders(path)
      (remainders, queries, result)
    override def decodeWithRemainders(
        path: Chain[String],
        queries: VectorMap[String, String]
    ): Validated[Violations, (Chain[String], VectorMap[String, String], A)] = this.path
      .decodeWithRemainders(path)
      .map { case (remainders, a) => (remainders, queries, a) }
      .leftMap(_.modifyHistory("path" /: _))
    override def encode(a: A): (Chain[String], VectorMap[String, String]) = (path.encode(a), VectorMap.empty)

  final private case class FromQueries[A](queries: Queries[A]) extends Url[A]:
    override def path: Path[?] = Path.Root
    override def matchesWithRemainders(
        path: Chain[String],
        queries: VectorMap[String, String]
    ): (Chain[String], VectorMap[String, String], Boolean) =
      val (remainders, result) = this.queries.matchesWithRemainders(queries)
      (path, remainders, result)
    override def decodeWithRemainders(
        path: Chain[String],
        queries: VectorMap[String, String]
    ): Validated[Violations, (Chain[String], VectorMap[String, String], A)] = this.queries
      .decodeWithRemainders(queries)
      .map { case (remainders, a) => (path, remainders, a) }
      .leftMap(_.modifyHistory("queries" /: _))
    override def encode(a: A): (Chain[String], VectorMap[String, String]) = (Chain.empty, queries.encode(a))

  final private case class Product[A, B](left: Url[A], right: Url[B]) extends Url[(A, B)]:
    override def path: Path[?] = left.path.product(right.path)
    override def queries: Queries[?] = left.queries.product(right.queries)
    override def matchesWithRemainders(
        path: Chain[String],
        queries: VectorMap[String, String]
    ): (Chain[String], VectorMap[String, String], Boolean) =
      val (pathRemainders1, queryRemainders1, result1) = left.matchesWithRemainders(path, queries)
      val (pathRemainders2, queryRemainders2, result2) = right.matchesWithRemainders(pathRemainders1, queryRemainders1)
      (pathRemainders2, queryRemainders2, result1 && result2)
    override def decodeWithRemainders(
        path: Chain[String],
        queries: VectorMap[String, String]
    ): Validated[Violations, (Chain[String], VectorMap[String, String], (A, B))] =
      left.decodeWithRemainders(path, queries).andThen { case (pathRemainders, queryRemainders, a) =>
        right.decodeWithRemainders(pathRemainders, queryRemainders).map(_.tupleLeft(a))
      }
    override def encode(ab: (A, B)): (Chain[String], VectorMap[String, String]) =
      val (path1, queries1) = left.encode(ab._1)
      val (path2, queries2) = right.encode(ab._2)
      (path1 ++ path2, queries1 ++ queries2)

  final private case class Modify[A, B](url: Url[A], f: A => B, g: B => A) extends Url[B]:
    override def path: Path[?] = url.path
    override def queries: Queries[?] = url.queries
    override def matchesWithRemainders(
        path: Chain[String],
        queries: VectorMap[String, String]
    ): (Chain[String], VectorMap[String, String], Boolean) =
      url.matchesWithRemainders(path, queries)
    override def decodeWithRemainders(
        path: Chain[String],
        queries: VectorMap[String, String]
    ): Validated[Violations, (Chain[String], VectorMap[String, String], B)] =
      url.decodeWithRemainders(path, queries).map(_.map(f))
    override def encode(b: B): (Chain[String], VectorMap[String, String]) = url.encode(g(b))

  val Root: Url[Void] = new Url[Void]:
    override def path: Path[?] = Path.Root
    override def queries: Queries[?] = Queries.Empty
    override def matchesWithRemainders(
        path: Chain[String],
        queries: VectorMap[String, String]
    ): (Chain[String], VectorMap[String, String], Boolean) = (path, queries, true)
    override def decodeWithRemainders(
        path: Chain[String],
        queries: VectorMap[String, String]
    ): Validated[Violations, (Chain[String], VectorMap[String, String], Void)] = (path, queries, Void).valid
    override def encode(a: Void): (Chain[String], VectorMap[String, String]) = (Chain.empty, VectorMap.empty)

  def apply[A](path: Path[A]): Url[A] = FromPath(path)
  def apply[A](queries: Queries[A]): Url[A] = FromQueries(queries)
