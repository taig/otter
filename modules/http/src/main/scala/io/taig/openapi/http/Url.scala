package io.taig.openapi.http

import cats.data.{Chain, Validated}
import cats.syntax.all.*
import io.taig.openapi.{Encoder, History, OpenApi}
import io.taig.openapi.syntax.*
import io.taig.openapi.schema.{Violations, Void}
import io.taig.openapi.validation.Constraint

import scala.collection.immutable.VectorMap

sealed abstract class Url[A]:
  def path: Path[?]
  def queries: Queries[?]
  def matches(path: Chain[String], queries: VectorMap[String, String]): Boolean
  final def product[B](url: Url[B]): Url[(A, B)] = Url.Product(this, url)
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

  val Root: Url[Void] = new Url[Void]:
    override def path: Path[?] = Path.Root
    override def queries: Queries[?] = Queries.Empty
    override def matches(path: Chain[String], queries: VectorMap[String, String]): Boolean =
      path.isEmpty
    override def decodeWithRemainders(
        path: Chain[String],
        queries: VectorMap[String, String]
    ): Validated[Violations, (Chain[String], VectorMap[String, String], Void)] = Validated.cond(
      matches(path, queries),
      (path, queries, Void),
      Violations.oneNec(
        History.Root / "path",
        Constraint.text.equal(OpenApi.fromString("/")).toViolation(renderPath(path).asOpenApi)
      )
    )
    override def encode(a: Void): (Chain[String], VectorMap[String, String]) =
      (Chain.empty, VectorMap.empty)

  final private case class Product[A, B](left: Url[A], right: Url[B]) extends Url[(A, B)]:
    override def path: Path[?] = left.path.product(right.path)
    override def queries: Queries[?] = left.queries.product(right.queries)
    override def matches(path: Chain[String], queries: VectorMap[String, String]): Boolean = ???
    override def decodeWithRemainders(
        path: Chain[String],
        queries: VectorMap[String, String]
    ): Validated[Violations, (Chain[String], VectorMap[String, String], (A, B))] = ???
    override def encode(ab: (A, B)): (Chain[String], VectorMap[String, String]) =
      val (path1, queries1) = left.encode(ab._1)
      val (path2, queries2) = right.encode(ab._2)
      (path1 ++ path2, queries1 ++ queries2)

  final private case class Modify[A, B](url: Url[A], f: A => B, g: B => A) extends Url[B]:
    override def path: Path[?] = url.path
    override def queries: Queries[?] = url.queries
    override def matches(path: Chain[String], queries: VectorMap[String, String]): Boolean =
      url.matches(path, queries)
    override def decodeWithRemainders(
        path: Chain[String],
        queries: VectorMap[String, String]
    ): Validated[Violations, (Chain[String], VectorMap[String, String], B)] =
      url.decodeWithRemainders(path, queries).map(_.map(f))
    override def encode(b: B): (Chain[String], VectorMap[String, String]) = url.encode(g(b))
