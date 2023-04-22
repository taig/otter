package io.taig.openapi.http

import cats.data.{Chain, Validated}
import cats.syntax.all.*
import io.taig.openapi.schema.{andThenValidate, Evidence, InvariantValidation, Violations}
import io.taig.openapi.{Encoder, OpenApi}
import io.taig.validation.Validation

sealed abstract class Url[A](val path: Path[?], val queries: Queries[?]):
  self =>

  final def imap[B](f: A => B)(g: B => A): Url[B] = new Url[B](path, queries):
    override def decodeWithRemainders(
        path: Chain[OpenApi.Primitive],
        queries: Chain[(String, OpenApi.Primitive)]
    ): Validated[Violations, (Chain[OpenApi.Primitive], Chain[(String, OpenApi.Primitive)], B)] =
      self.decodeWithRemainders(path, queries).map(_.map(f))
    override def encode(b: B): (Chain[OpenApi.Primitive], Chain[(String, OpenApi.Primitive)]) = self.encode(g(b))
    override def render(b: B): (Chain[String], Chain[String]) = self.render(g(b))

  final def gimap[B](using evidence: Evidence.Product.Aux[B, A]): Url[B] = imap(evidence.from)(evidence.to)
  final def ivalidate[B: Encoder, C](validation: Validation[B, A, A, C])(g: C => A): Url[C] = new Url[C](path, queries):
    override def decodeWithRemainders(
        path: Chain[OpenApi.Primitive],
        queries: Chain[(String, OpenApi.Primitive)]
    ): Validated[Violations, (Chain[OpenApi.Primitive], Chain[(String, OpenApi.Primitive)], C)] = self
      .decodeWithRemainders(path, queries)
      .andThen(_.traverse(andThenValidate(validation, a => OpenApi.fromString(printUrl.tupled(self.render(a))))))
    override def encode(c: C): (Chain[OpenApi.Primitive], Chain[(String, OpenApi.Primitive)]) = self.encode(g(c))
    override def render(c: C): (Chain[String], Chain[String]) = self.render(g(c))

  def zipQueries[B](values: Queries[B]): Url[(A, B)] = new Url[(A, B)](path, this.queries zip values):
    override def decodeWithRemainders(
        path: Chain[OpenApi.Primitive],
        queries: Chain[(String, OpenApi.Primitive)]
    ): Validated[Violations, (Chain[OpenApi.Primitive], Chain[(String, OpenApi.Primitive)], (A, B))] =
      self.decodeWithRemainders(path, queries) match
        case Validated.Valid((path, queries, a)) =>
          values.decodeWithRemainders(queries).map { case (queries, b) => (path, queries, (a, b)) }
        case Validated.Invalid(violations) =>
          values.decodeWithRemainders(queries).fold(violations |+| _, _ => violations).invalid

    override def encode(ab: (A, B)): (Chain[OpenApi.Primitive], Chain[(String, OpenApi.Primitive)]) =
      self.encode(ab._1).map(_ ++ values.encode(ab._2))

    override def render(ab: (A, B)): (Chain[String], Chain[String]) = self.render(ab._1).map(_ ++ values.render(ab._2))

  def :&[B](query: Query[B]): Url[(A, B)] = zipQueries(query.toQueries)

  def matches(path: Chain[OpenApi.Primitive], queries: Chain[(String, OpenApi.Primitive)]): Boolean =
    this.path.matches(path) && this.queries.matches(queries).isDefined

  final def decode(
      path: Chain[OpenApi.Primitive],
      queries: Chain[(String, OpenApi.Primitive)]
  ): Validated[Violations, A] = decodeWithRemainders(path, queries).map(_._3)

  def decodeWithRemainders(
      path: Chain[OpenApi.Primitive],
      queries: Chain[(String, OpenApi.Primitive)]
  ): Validated[Violations, (Chain[OpenApi.Primitive], Chain[(String, OpenApi.Primitive)], A)]

  def encode(a: A): (Chain[OpenApi.Primitive], Chain[(String, OpenApi.Primitive)])

  def render(a: A): (Chain[String], Chain[String])

object Url:
  val Empty: Url[Unit] = new Url[Unit](Path.Root, Queries.Empty):
    override def decodeWithRemainders(
        path: Chain[OpenApi.Primitive],
        queries: Chain[(String, OpenApi.Primitive)]
    ): Validated[Violations, (Chain[OpenApi.Primitive], Chain[(String, OpenApi.Primitive)], Unit)] =
      (path, queries, ()).valid
    override def encode(a: Unit): (Chain[OpenApi.Primitive], Chain[(String, OpenApi.Primitive)]) =
      (Chain.empty, Chain.empty)
    override def render(a: Unit): (Chain[String], Chain[String]) = (Chain.empty, Chain.empty)

  def apply[A, B](_path: Path[A], _queries: Queries[B]): Url[(A, B)] = new Url[(A, B)](_path, _queries):
    override def decodeWithRemainders(
        path: Chain[OpenApi.Primitive],
        queries: Chain[(String, OpenApi.Primitive)]
    ): Validated[Violations, (Chain[OpenApi.Primitive], Chain[(String, OpenApi.Primitive)], (A, B))] =
      (_path.decodeWithRemainders(path), _queries.decodeWithRemainders(queries)).mapN {
        case ((path, a), (queries, b)) => (path, queries, (a, b))
      }

    override def encode(ab: (A, B)): (Chain[OpenApi.Primitive], Chain[(String, OpenApi.Primitive)]) =
      (_path.encode(ab._1), _queries.encode(ab._2))
    override def render(ab: (A, B)): (Chain[String], Chain[String]) = (_path.render(ab._1), _queries.render(ab._2))

  def fromPath[A](path: Path[A]): Url[A] = Url(path, Queries.Empty).imap { case (a, _) => a }(a => (a, ()))

  def fromQueries[A](queries: Queries[A]): Url[A] = Url(Path.Root, queries).imap { case (_, a) => a }(a => ((), a))

  given InvariantValidation[Url] with
    override def imap[A, B](fa: Url[A])(f: A => B)(g: B => A): Url[B] = fa.imap(f)(g)
    override def ivalidate[A: Encoder, B, C](fa: Url[B])(validation: Validation[A, B, B, C])(g: C => B): Url[C] =
      fa.ivalidate(validation)(g)
