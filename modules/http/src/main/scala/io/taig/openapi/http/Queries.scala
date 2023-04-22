package io.taig.openapi.http

import cats.data.{Chain, Validated}
import cats.syntax.all.*
import io.taig.openapi.{Encoder, OpenApi}
import io.taig.openapi.schema.{andThenValidate, Evidence, InvariantValidation, Violations}
import io.taig.validation.{Validation, Violation}

sealed abstract class Queries[A](val toChain: Chain[Query[?]]):
  self =>

  final def imap[B](f: A => B)(g: B => A): Queries[B] = new Queries[B](toChain):
    override def decodeWithRemainders(
        queries: Chain[(String, OpenApi.Primitive)]
    ): Validated[Violations, (Chain[(String, OpenApi.Primitive)], B)] =
      self.decodeWithRemainders(queries).map(_.map(f))
    override def encode(b: B): Chain[(String, OpenApi.Primitive)] = self.encode(g(b))
    override def render(b: B): Chain[String] = self.render(g(b))

  final def gimap[B](using evidence: Evidence.Product.Aux[B, A]): Queries[B] = imap(evidence.from)(evidence.to)
  final def ivalidate[B: Encoder, C](validation: Validation[B, A, A, C])(g: C => A): Queries[C] =
    new Queries[C](toChain):
      override def decodeWithRemainders(
          queries: Chain[(String, OpenApi.Primitive)]
      ): Validated[Violations, (Chain[(String, OpenApi.Primitive)], C)] = self
        .decodeWithRemainders(queries)
        .andThen(
          _.traverse(andThenValidate(validation, a => OpenApi.fromChain(self.render(a).map(OpenApi.fromString))))
        )

      override def encode(b: C): Chain[(String, OpenApi.Primitive)] = self.encode(g(b))
      override def render(b: C): Chain[String] = self.render(g(b))

  final infix def zip[B](queries: Queries[B]): Queries[(A, B)] = new Queries[(A, B)](toChain):
    override def decodeWithRemainders(
        values: Chain[(String, OpenApi.Primitive)]
    ): Validated[Violations, (Chain[(String, OpenApi.Primitive)], (A, B))] = self.decodeWithRemainders(values) match
      case Validated.Valid((remainders, a)) =>
        queries.decodeWithRemainders(remainders).map { case (remainders, b) => (remainders, (a, b)) }
      case Validated.Invalid(violations) =>
        queries.decodeWithRemainders(values).fold(violations |+| _, _ => violations).invalid
    override def encode(ab: (A, B)): Chain[(String, OpenApi.Primitive)] = self.encode(ab._1) ++ queries.encode(ab._2)
    override def render(ab: (A, B)): Chain[String] = self.render(ab._1) ++ queries.render(ab._2)

  final def :&[B](query: Query[B]): Queries[(A, B)] = zip(query.toQueries)

  final def matches(queries: Chain[(String, OpenApi.Primitive)]): Option[Chain[(String, OpenApi.Primitive)]] =
    toChain.foldLeft(queries.some) {
      case (Some(queries), query) =>
        query.schema.value match
          case Query.Value.Required(_) =>
            val (remainders, result) = collectAndRemoveFirst(queries) { case (name, _) if query.name === name => () }
            result.as(remainders)
          case Query.Value.Optional(_) =>
            val (remainders, _) = collectAndRemoveFirst(queries) { case (name, _) if query.name === name => () }
            remainders.some
      case (None, _) => None
    }

  final def decode(values: Chain[(String, OpenApi.Primitive)]): Validated[Violations, A] =
    decodeWithRemainders(values).map(_._2)

  def decodeWithRemainders(
      queries: Chain[(String, OpenApi.Primitive)]
  ): Validated[Violations, (Chain[(String, OpenApi.Primitive)], A)]

  def encode(a: A): Chain[(String, OpenApi.Primitive)]

  def render(a: A): Chain[String]

  final def toUrl: Url[A] = Url.fromQueries(this)

object Queries:
  val Empty: Queries[Unit] = new Queries[Unit](Chain.empty):
    override def decodeWithRemainders(
        queries: Chain[(String, OpenApi.Primitive)]
    ): Validated[Violations, (Chain[(String, OpenApi.Primitive)], Unit)] = (queries, ()).valid
    override def encode(a: Unit): Chain[(String, OpenApi.Primitive)] = Chain.empty
    override def render(a: Unit): Chain[String] = Chain.empty

  def one[A](query: Query[A]): Queries[A] = new Queries[A](Chain.one(query)):
    override def decodeWithRemainders(
        queries: Chain[(String, OpenApi.Primitive)]
    ): Validated[Violations, (Chain[(String, OpenApi.Primitive)], A)] = query.decode(queries)
    override def encode(a: A): Chain[(String, OpenApi.Primitive)] = query.encode(a)
    override def render(a: A): Chain[String] = query.render(a)

  given InvariantValidation.Product[Queries] with
    override def unit: Queries[Unit] = Empty
    override def product[A, B](fa: Queries[A], fb: Queries[B]): Queries[(A, B)] = fa zip fb
    override def imap[A, B](fa: Queries[A])(f: A => B)(g: B => A): Queries[B] = fa.imap(f)(g)
    override def ivalidate[A: Encoder, B, C](fa: Queries[B])(validation: Validation[A, B, B, C])(
        g: C => B
    ): Queries[C] =
      fa.ivalidate(validation)(g)
