package io.taig.openapi.http

import cats.data.{Chain, Validated}
import cats.syntax.all.*
import io.taig.openapi.{Encoder, History, OpenApi}
import io.taig.openapi.syntax.*
import io.taig.openapi.schema.{Violations, Void}
import io.taig.openapi.schema.applyValidation
import io.taig.openapi.validation.{Constraint, Validation}

import scala.collection.immutable.VectorMap

sealed abstract class Url[A]:
  def constraint: Chain[Constraint[OpenApi]]
  def matches(path: Chain[OpenApi.Primitive], queries: VectorMap[String, OpenApi.Primitive]): Boolean
  final def product[B](segment: Segment[B]): Url[(A, B)] = Url.ProductSegment(this, segment)
  final def product[B](query: Query[B]): Url[(A, B)] = Url.ProductQuery(this, query)
  final transparent inline def /[B](segment: Segment[B]): Url[?] = inline (this, segment) match
    case (left: Url[Void], right)     => left.product(right).imap[B] { case (_, b) => b }(b => (Void, b))
    case (left, right: Segment[Void]) => left.product(right).imap[A] { case (a, _) => a }(a => (a, Void))
    case (left: Url[? *: ?], right) =>
      left.product(right).imap { case (a, b) => a :* b }(ab => (ab.init.asInstanceOf[A], ab.last.asInstanceOf[B]))
    case (left, right) => left.product(right)
  final def /(segment: String): Url[A] = product(Segment.static(segment)).imap { case (a, _) => a }(a => (a, Void))
  final transparent inline def &[B](query: Query[B]): Url[?] = inline (this, query) match
    case (left: Url[Void], right)   => left.product(right).imap[B] { case (_, b) => b }(b => (Void, b))
    case (left, right: Query[Void]) => left.product(right).imap[A] { case (a, _) => a }(a => (a, Void))
    case (left: Url[? *: ?], right) =>
      left.product(right).imap { case (a, b) => a :* b } { ab => (ab.init.asInstanceOf[A], ab.last.asInstanceOf[B]) }
    case (left, right) => left.product(right)
  final def ivalidate[B: Encoder, C](validation: Validation[B, A, A, C])(g: C => A): Url[C] =
    Url.Validate(this, validation, g)
  final def imap[B](f: A => B)(g: B => A): Url[B] = ivalidate(Validation.lift(f))(g)
  final def decode(
      path: Chain[OpenApi.Primitive],
      queries: VectorMap[String, OpenApi.Primitive]
  ): Validated[Violations, A] = decodeWithRemainders(path, queries).map(_._3)
  def decodeWithRemainders(
      path: Chain[OpenApi.Primitive],
      queries: VectorMap[String, OpenApi.Primitive]
  ): Validated[Violations, (Chain[OpenApi.Primitive], VectorMap[String, OpenApi.Primitive], A)]
  def encode(a: A): (Chain[OpenApi.Primitive], VectorMap[String, OpenApi.Primitive])

object Url:
  private def renderPath(path: Chain[OpenApi.Primitive]): String =
    path.map(_.render).mkString_("/")
  private def renderQueries(queries: VectorMap[String, OpenApi.Primitive]): String =
    queries.toSeq.map { case (name, value) => s"$name=${value.render}" }.mkString_("&")
  private def renderUrl(path: Chain[OpenApi.Primitive], queries: VectorMap[String, OpenApi.Primitive]): String =
    if queries.isEmpty then renderPath(path) else renderPath(path) + "?" + renderQueries(queries)

  val Root: Url[Void] = new Url[Void]:
    override def constraint: Chain[Constraint[OpenApi]] = Chain.empty
    override def matches(path: Chain[OpenApi.Primitive], queries: VectorMap[String, OpenApi.Primitive]): Boolean =
      path.isEmpty
    override def decodeWithRemainders(
        path: Chain[OpenApi.Primitive],
        queries: VectorMap[String, OpenApi.Primitive]
    ): Validated[Violations, (Chain[OpenApi.Primitive], VectorMap[String, OpenApi.Primitive], Void)] =
      Validated.cond(
        matches(path, queries),
        (path, queries, Void),
        Violations.rootNec(Constraint.text.equal("/").toViolation(renderPath(path).asOpenApi).mapReference(_.asOpenApi))
      )
    override def encode(a: Void): (Chain[OpenApi.Primitive], VectorMap[String, OpenApi.Primitive]) =
      (Chain.empty, VectorMap.empty)

  final private case class ProductSegment[A, B](url: Url[A], segment: Segment[B]) extends Url[(A, B)]:
    override def constraint: Chain[Constraint[OpenApi]] = url.constraint
    override def matches(path: Chain[OpenApi.Primitive], queries: VectorMap[String, OpenApi.Primitive]): Boolean =
      path.initLast match
        case Some((init, last)) => url.matches(init, queries) && segment.matches(last)
        case None               => false
    override def decodeWithRemainders(
        path: Chain[OpenApi.Primitive],
        queries: VectorMap[String, OpenApi.Primitive]
    ): Validated[Violations, (Chain[OpenApi.Primitive], VectorMap[String, OpenApi.Primitive], (A, B))] =
      url.decodeWithRemainders(path, queries).andThen { case (path, queries, a) =>
        path.uncons match
          case Some((head, tail)) =>
            if tail.isEmpty
            then segment.decode(head).map(b => (tail, queries, (a, b)))
            else
              val actual = renderPath(tail).asOpenApi
              val violation = Constraint.text.equal("/").toViolation(actual).mapReference(_.asOpenApi)
              Violations.rootNec(violation).invalid
          case None =>
            Violations.oneNec(History.Root / segment.name, Constraint.required.toViolation(OpenApi.Null)).invalid
      }
    override def encode(ab: (A, B)): (Chain[OpenApi.Primitive], VectorMap[String, OpenApi.Primitive]) =
      val (segments, queries) = url.encode(ab._1)
      (segments :+ segment.encode(ab._2), queries)

  final private case class ProductQuery[A, B](url: Url[A], query: Query[B]) extends Url[(A, B)]:
    override def constraint: Chain[Constraint[OpenApi]] = url.constraint
    override def matches(path: Chain[OpenApi.Primitive], queries: VectorMap[String, OpenApi.Primitive]): Boolean =
      if query.isOptional then true else queries.contains(query.name)
    override def decodeWithRemainders(
        path: Chain[OpenApi.Primitive],
        queries: VectorMap[String, OpenApi.Primitive]
    ): Validated[Violations, (Chain[OpenApi.Primitive], VectorMap[String, OpenApi.Primitive], (A, B))] =
      url.decodeWithRemainders(path, queries).andThen { case (path, queries, a) =>
        query.decode(queries).map { case (remainders, b) => (path, remainders, (a, b)) }
      }
    override def encode(ab: (A, B)): (Chain[OpenApi.Primitive], VectorMap[String, OpenApi.Primitive]) =
      val (segments, queries) = url.encode(ab._1)
      (segments, queries ++ query.encode(ab._2))

  final private case class Validate[A, B: Encoder, C](url: Url[A], validation: Validation[B, A, A, C], g: C => A)
      extends Url[C]:
    override def constraint: Chain[Constraint[OpenApi]] =
      url.constraint ++ validation.constraints.map(_.map(_.asOpenApi))
    override def matches(path: Chain[OpenApi.Primitive], queries: VectorMap[String, OpenApi.Primitive]): Boolean =
      url.matches(path, queries)
    override def decodeWithRemainders(
        path: Chain[OpenApi.Primitive],
        queries: VectorMap[String, OpenApi.Primitive]
    ): Validated[Violations, (Chain[OpenApi.Primitive], VectorMap[String, OpenApi.Primitive], C)] =
      url
        .decodeWithRemainders(path, queries)
        .andThen(_.traverse(applyValidation(validation, a => renderUrl.tupled(url.encode(a)).asOpenApi)))
    override def encode(c: C): (Chain[OpenApi.Primitive], VectorMap[String, OpenApi.Primitive]) = url.encode(g(c))
