package io.taig.openapi.http

import cats.data.{Chain, Validated}
import cats.syntax.all.*
import io.taig.openapi.{Encoder, History, OpenApi}
import io.taig.openapi.syntax.*
import io.taig.openapi.schema.{Violations, Void}
import io.taig.openapi.validation.Constraint

import scala.collection.immutable.VectorMap

sealed abstract class Url[A]:
  def matches(path: Chain[String], queries: VectorMap[String, String]): Boolean
  final def product[B](segment: Segment[B]): Url[(A, B)] = Url.ProductSegment(this, segment)
  final transparent inline def /[B](segment: Segment[B]): Url[?] = inline (this, segment) match
    case (left: Url[Void], right)     => left.product(right).imap[B] { case (_, b) => b }(b => (Void, b))
    case (left, right: Segment[Void]) => left.product(right).imap[A] { case (a, _) => a }(a => (a, Void))
    case (left: Url[? *: ?], right) =>
      left.product(right).imap { case (a, b) => a :* b }(ab => (ab.init.asInstanceOf[A], ab.last.asInstanceOf[B]))
    case (left, right) => left.product(right)
  final def /(segment: String): Url[A] = product(Segment.Static(segment)).imap { case (a, _) => a }(a => (a, Void))
  final def product[B](queries: Queries[B]): Url[(A, B)] = Url.ProductQueries(this, queries)
  final transparent inline def zip[B](queries: Queries[B]): Url[?] = inline (this, queries) match
    case (left: Url[Void], right)     => left.product(right).imap[B] { case (_, b) => b }(b => (Void, b))
    case (left, right: Queries[Void]) => left.product(right).imap[A] { case (a, _) => a }(a => (a, Void))
    case (left: Url[? *: ?], right) =>
      left.product(right).imap { case (a, b) => a :* b } { ab => (ab.init.asInstanceOf[A], ab.last.asInstanceOf[B]) }
    case (left, right) => left.product(right)
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

  val Root: Url[Void] = new Url[Void]:
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

  final private case class ProductSegment[A, B](url: Url[A], segment: Segment[B]) extends Url[(A, B)]:
    override def matches(path: Chain[String], queries: VectorMap[String, String]): Boolean =
      path.initLast match
        case Some((init, last)) => url.matches(init, queries) && segment.matches(last)
        case None               => false
    override def decodeWithRemainders(
        path: Chain[String],
        queries: VectorMap[String, String]
    ): Validated[Violations, (Chain[String], VectorMap[String, String], (A, B))] = path.initLast match
      case Some((init, last)) =>
        segment.decode(last).andThen(b => url.decodeWithRemainders(init, queries).map(_.tupleRight(b)))
      case None =>
        Violations
          .oneNec(History.Root / "path" / segment.name, Constraint.required.toViolation("/".asOpenApi))
          .invalid
    override def encode(ab: (A, B)): (Chain[String], VectorMap[String, String]) =
      val (segments, queries) = url.encode(ab._1)
      (segments :+ segment.encode(ab._2), queries)

  final private case class ProductQueries[A, B](url: Url[A], queries: Queries[B]) extends Url[(A, B)]:
    override def matches(path: Chain[String], queries: VectorMap[String, String]): Boolean =
      url.matches(path, queries) && this.queries.matches(queries)
    override def decodeWithRemainders(
        path: Chain[String],
        queries: VectorMap[String, String]
    ): Validated[Violations, (Chain[String], VectorMap[String, String], (A, B))] =
      url.decodeWithRemainders(path, queries).andThen { case (path, queries, a) =>
        this.queries.decodeWithRemainders(queries).map { case (remainders, b) => (path, remainders, (a, b)) }
      }
    override def encode(ab: (A, B)): (Chain[String], VectorMap[String, String]) =
      val (segments, queries) = url.encode(ab._1)
      (segments, queries ++ this.queries.encode(ab._2))

  final private case class Modify[A, B](url: Url[A], f: A => B, g: B => A) extends Url[B]:
    override def matches(path: Chain[String], queries: VectorMap[String, String]): Boolean =
      url.matches(path, queries)
    override def decodeWithRemainders(
        path: Chain[String],
        queries: VectorMap[String, String]
    ): Validated[Violations, (Chain[String], VectorMap[String, String], B)] =
      url.decodeWithRemainders(path, queries).map(_.map(f))
    override def encode(b: B): (Chain[String], VectorMap[String, String]) = url.encode(g(b))
