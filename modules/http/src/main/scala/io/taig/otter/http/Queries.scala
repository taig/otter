package io.taig.otter.http

import cats.syntax.all.*
import io.taig.otter.Codec
import io.taig.otter.Convert
import io.taig.otter.collectFirstWithRemainders
import io.taig.otter.Merge
import cats.data.Validated
import cats.data.Validated.Valid

sealed abstract class Queries[A]:
  self =>

  def toVector: Vector[Query[?]]

  final def matches(queries: Http.Queries): Boolean = matchesRemainders(queries).isDefined

  protected def matchesRemainders(queries: Http.Queries): Option[Http.Queries]

  final def imap[B](f: A => B)(g: B => A): Queries[B] = new Queries[B]:
    export self.{matchesRemainders, toVector}
    override def decode(values: Http.Queries): (Http.Queries, Codec.Result[B]) = self.decode(values).map(_.map(f))
    override def encode(b: B): Http.Queries = self.encode(g(b))

  final def to[B](using convert: Convert[A, B]): Queries[B] = imap(convert.to)(convert.from)

  final def zip[B](queries: Queries[B]): Queries[(A, B)] = new Queries[(A, B)]:
    override def toVector: Vector[Query[?]] = self.toVector ++ queries.toVector
    override def matchesRemainders(values: Http.Queries): Option[Http.Queries] =
      self.matchesRemainders(values).flatMap(queries.matchesRemainders)
    override def decode(values: Http.Queries): (Http.Queries, Codec.Result[(A, B)]) =
      self.decode(values) match
        case (values, Validated.Valid(a)) =>
          queries.decode(values) match
            case (values, Validated.Valid(b))            => (values, (a, b).valid)
            case (values, Validated.Invalid(violations)) => (values, violations.invalid)
        case (values, Validated.Invalid(left)) =>
          queries.decode(values) match
            case (values, Validated.Valid(_))       => (values, left.invalid)
            case (values, Validated.Invalid(right)) => (values, (left |+| right).invalid)
    override def encode(ab: (A, B)): Http.Queries = self.encode(ab._1) ++ queries.encode(ab._2)

  final def optional: Queries[Option[A]] = new Queries[Option[A]]:
    export self.toVector
    override def matchesRemainders(queries: Http.Queries): Option[Http.Queries] = queries.some
    override def decode(values: Http.Queries): (Http.Queries, Codec.Result[Option[A]]) =
      val availableNames = values.map { case (name, _) => name }
      val requiredNames = toVector.filter(_.isRequired).map(_.name)
      val isOptional = requiredNames.forall(name => !availableNames.contains(name))
      if isOptional then (values, none.valid) else self.decode(values).map(_.map(_.some))
    override def encode(a: Option[A]): Http.Queries = a.fold(Vector.empty)(self.encode)

  final def :*[B](query: Query[B])(using merge: Merge[A, B]): Queries[merge.Out] =
    zip(query.toQueries).imap(merge.apply)(merge.unapply)

  final def *:[B](query: Query[B])(using merge: Merge[B, A]): Queries[merge.Out] =
    query.toQueries.zip(this).imap(merge.apply)(merge.unapply)

  final def toUrl: Url[A] = Url(this)

  def decode(values: Http.Queries): (Http.Queries, Codec.Result[A])

  def encode(a: A): Http.Queries

object Queries:
  val Empty: Queries[Unit] = new Queries[Unit]:
    override def toVector: Vector[Query[?]] = Vector.empty
    override def matchesRemainders(queries: Http.Queries): Option[Http.Queries] = queries.some
    override def decode(values: Http.Queries): (Http.Queries, Codec.Result[Unit]) = (values, ().valid)
    override def encode(a: Unit): Http.Queries = Vector.empty

  def apply[A](query: Query[A]): Queries[A] = new Queries[A]:
    override def toVector: Vector[Query[?]] = Vector(query)
    override def matchesRemainders(queries: Http.Queries): Option[Http.Queries] =
      if query.isNullable then queries.some
      else
        val (remainders, result) = queries.collectFirstWithRemainders { case (name, _) if name === query.name => () }
        result.as(remainders)
    override def decode(values: Http.Queries): (Http.Queries, Codec.Result[A]) = query.decode(values)
    override def encode(a: A): Http.Queries = query.encode(a)
