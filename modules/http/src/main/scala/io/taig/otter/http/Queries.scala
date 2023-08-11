package io.taig.otter.http

import cats.InvariantSemigroupal
import cats.data.{Chain, Validated}
import cats.syntax.all.*
import io.taig.otter.schema.Violations

sealed abstract class Queries[A]:
  self =>
  def toChain: Chain[Query[?]]

  final def imap[B](f: A => B)(g: B => A): Queries[B] = new Queries[B]:
    export self.{matchesWithRemainders, toChain}
    override def decodeWithRemainders(remainders: Http.Queries): Validated[Violations, (Http.Queries, B)] =
      self.decodeWithRemainders(remainders).map(_.map(f))
    override def encode(b: B): Http.Queries = self.encode(g(b))

  final def zip[B](queries: Queries[B]): Queries[(A, B)] = new Queries[(A, B)]:
    override def toChain: Chain[Query[?]] = self.toChain ++ queries.toChain
    override def matchesWithRemainders(remainders: Http.Queries): Option[Http.Queries] =
      self.matchesWithRemainders(remainders).flatMap(queries.matchesWithRemainders)
    override def decodeWithRemainders(remainders: Http.Queries): Validated[Violations, (Http.Queries, (A, B))] =
      self.decodeWithRemainders(remainders) match
        case Validated.Valid((remainders, a)) => queries.decodeWithRemainders(remainders).map(_.tupleLeft(a))
        case Validated.Invalid(left) =>
          queries.decodeWithRemainders(remainders) match
            case Validated.Valid(_)       => left.invalid
            case Validated.Invalid(right) => (left |+| right).invalid
    override def encode(ab: (A, B)): Http.Queries = self.encode(ab._1) ++ queries.encode(ab._2)

  final def matches(queries: Http.Queries): Boolean = matchesWithRemainders(queries).isDefined
  def matchesWithRemainders(remainders: Http.Queries): Option[Http.Queries]

  final def decode(queries: Http.Queries): Validated[Violations, A] = decodeWithRemainders(queries).map(_._2)
  def decodeWithRemainders(remainders: Http.Queries): Validated[Violations, (Http.Queries, A)]
  def encode(a: A): Http.Queries

  final def toUrl: Url[A] = Url(this)

object Queries:
  val Empty: Queries[Unit] = new Queries[Unit]:
    override def toChain: Chain[Query[?]] = Chain.empty
    override def matchesWithRemainders(remainders: Http.Queries): Option[Http.Queries] = remainders.some
    override def decodeWithRemainders(remainders: Http.Queries): Validated[Violations, (Http.Queries, Unit)] =
      (remainders, ()).valid
    override def encode(a: Unit): Http.Queries = Chain.empty
  def apply[A](query: Query[A]): Queries[A] = new Queries[A]:
    override def toChain: Chain[Query[?]] = Chain.one(query)
    override def matchesWithRemainders(remainders: Http.Queries): Option[Http.Queries] =
      query.matchesWithRemainders(remainders)
    override def decodeWithRemainders(remainders: Http.Queries): Validated[Violations, (Http.Queries, A)] =
      query.decodeWithRemainders(remainders)
    override def encode(a: A): Http.Queries = query.encode(a)

  given InvariantSemigroupal[Queries] with
    override def imap[A, B](fa: Queries[A])(f: A => B)(g: B => A): Queries[B] = fa.imap(f)(g)
    override def product[A, B](fa: Queries[A], fb: Queries[B]): Queries[(A, B)] = fa.zip(fb)
