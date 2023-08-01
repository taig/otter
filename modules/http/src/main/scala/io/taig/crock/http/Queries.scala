package io.taig.crock.http

import cats.InvariantSemigroupal
import cats.data.{Chain, Validated}
import cats.syntax.all.*

sealed abstract class Queries[A]:
  def toChain: Chain[Query[?]]
  final def matches(queries: Http.Queries): Boolean = matchesWithRemainders(queries)._2
  def matchesWithRemainders(queries: Http.Queries): (Http.Queries, Boolean)
  final def product[B](queries: Queries[B]): Queries[(A, B)] = Queries.Product(this, queries)
  final transparent inline def zip[B](queries: Queries[B]): Queries[?] = inline (this, queries) match
    case (left: Queries[Unit], right) => left.product(right).imap[B] { case (_, b) => b }(b => ((), b))
    case (left, right: Queries[Unit]) => left.product(right).imap[A] { case (a, _) => a }(a => (a, ()))
    case (left: Queries[? *: ?], right) =>
      left.product(right).imap { case (a, b) => a :* b }(ab => (ab.init.asInstanceOf[A], ab.last.asInstanceOf[B]))
    case (left, right) => left.product(right)
  final transparent inline def &[B](query: Query[B]): Queries[?] = zip(query.toQueries)
  final def toUrl: Url[A] = Url(this)
  final def imap[B](f: A => B)(g: B => A): Queries[B] = Queries.Modify(this, f, g)
  final def decode(values: Http.Queries): Validated[Violations, A] =
    decodeWithRemainders(values).map(_._2)
  def decodeWithRemainders(
      queries: Http.Queries
  ): Validated[Violations, (Http.Queries, A)]
  def encode(a: A): Http.Queries

object Queries:
  final private case class Root[A](query: Query[A]) extends Queries[A]:
    override def toChain: Chain[Query[?]] = Chain.one(query)
    override def matchesWithRemainders(queries: Http.Queries): (Http.Queries, Boolean) =
      (queries.remove(query.name), query.isOptional || queries.contains(query.name))
    override def decodeWithRemainders(
        queries: Http.Queries
    ): Validated[Violations, (Http.Queries, A)] = query.decode(queries).leftMap(_.modifyHistory(query.name /: _))
    override def encode(a: A): Http.Queries = query.encode(a)

  final private case class Product[A, B](left: Queries[A], right: Queries[B]) extends Queries[(A, B)]:
    override def toChain: Chain[Query[?]] = left.toChain ++ right.toChain
    override def matchesWithRemainders(queries: Http.Queries): (Http.Queries, Boolean) =
      val (remainders1, result1) = left.matchesWithRemainders(queries)
      val (remainders2, result2) = right.matchesWithRemainders(remainders1)
      (remainders2, result1 && result2)
    override def decodeWithRemainders(
        queries: Http.Queries
    ): Validated[Violations, (Http.Queries, (A, B))] = left.decodeWithRemainders(queries) match
      case Validated.Valid((remainders, a)) => right.decodeWithRemainders(remainders).map(_.tupleLeft(a))
      case Validated.Invalid(violations)    => right.decode(queries).fold(violations merge _, _ => violations).invalid
    override def encode(ab: (A, B)): Http.Queries = left.encode(ab._1) merge right.encode(ab._2)

  final private case class Modify[A, B](
      queries: Queries[A],
      f: A => B,
      g: B => A
  ) extends Queries[B]:
    override def toChain: Chain[Query[?]] = queries.toChain
    override def matchesWithRemainders(queries: Http.Queries): (Http.Queries, Boolean) =
      this.queries.matchesWithRemainders(queries)
    override def decodeWithRemainders(
        values: Http.Queries
    ): Validated[Violations, (Http.Queries, B)] = queries.decodeWithRemainders(values).map(_.map(f))
    override def encode(b: B): Http.Queries = queries.encode(g(b))

  val Empty: Queries[Unit] = new Queries[Unit]:
    override def toChain: Chain[Query[?]] = Chain.empty
    override def matchesWithRemainders(queries: Http.Queries): (Http.Queries, Boolean) =
      (queries, true)
    override def decodeWithRemainders(
        queries: Http.Queries
    ): Validated[Violations, (Http.Queries, Unit)] = (queries, ()).valid
    override def encode(a: Unit): Http.Queries = Http.Queries.Empty

  def apply[A](query: Query[A]): Queries[A] = Root(query)

  given InvariantSemigroupal[Queries] with
    override def imap[A, B](fa: Queries[A])(f: A => B)(g: B => A): Queries[B] = fa.imap(f)(g)
    override def product[A, B](fa: Queries[A], fb: Queries[B]): Queries[(A, B)] = fa.product(fb)
