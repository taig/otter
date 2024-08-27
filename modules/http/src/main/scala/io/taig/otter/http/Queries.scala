package io.taig.otter.http

import cats.syntax.all.*
import io.taig.otter.Codec
import io.taig.otter.filterKeys
import io.taig.otter.Merge
import io.taig.otter.Convert

sealed abstract class Queries[A]:
  self =>

  def toVector: Vector[Query[?]]

  def matches(queries: Http.Queries): Boolean

  final def imap[B](f: A => B)(g: B => A): Queries[B] = new Queries[B]:
    export self.{matches, toVector}
    override def decode(values: Http.Queries): Codec.Result[B] = self.decode(values).map(f)
    override def encode(b: B): Http.Queries = self.encode(g(b))

  final def to[B](using convert: Convert[A, B]): Queries[B] = imap(convert.to)(convert.from)

  final def zip[B](queries: Queries[B]): Queries[(A, B)] = new Queries[(A, B)]:
    override def toVector: Vector[Query[?]] = self.toVector ++ queries.toVector
    override def matches(values: Http.Queries): Boolean =
      val (left, remainders) = values.filterKeys(self.toVector.map(_.name))
      val (right, _) = remainders.filterKeys(queries.toVector.map(_.name))
      self.matches(left) && queries.matches(right)
    override def decode(values: Http.Queries): Codec.Result[(A, B)] =
      val (left, remainders) = values.filterKeys(self.toVector.map(_.name))
      val (right, _) = remainders.filterKeys(queries.toVector.map(_.name))
      (self.decode(left), queries.decode(right)).tupled
    override def encode(ab: (A, B)): Http.Queries = self.encode(ab._1) ++ queries.encode(ab._2)

  final def :*[B](query: Query[B])(using merge: Merge[A, B]): Queries[merge.Out] =
    zip(query.toQueries).imap(merge.apply)(merge.unapply)

  final def *:[B](query: Query[B])(using merge: Merge[B, A]): Queries[merge.Out] =
    query.toQueries.zip(this).imap(merge.apply)(merge.unapply)

  final def toUrl: Url[A] = Url(this)

  def decode(values: Http.Queries): Codec.Result[A]

  def encode(a: A): Http.Queries

object Queries:
  val Empty: Queries[Unit] = new Queries[Unit]:
    override def toVector: Vector[Query[?]] = Vector.empty
    override def matches(queries: Http.Queries): Boolean = true
    override def decode(values: Http.Queries): Codec.Result[Unit] = ().valid
    override def encode(a: Unit): Http.Queries = Vector.empty

  def apply[A](query: Query[A]): Queries[A] = new Queries[A]:
    override def toVector: Vector[Query[?]] = Vector(query)
    override def matches(queries: Http.Queries): Boolean =
      query.isOptional || queries.exists { case (key, _) => key === query.name }
    override def decode(values: Http.Queries): Codec.Result[A] =
      val value = values
        .collectFirst { case (key, value) if key === query.name => value }
        .fold(Query.Value.Abscent)(_.fold(Query.Value.None)(Query.Value.Some.apply))
      query.decode(value).leftMap(query.name /: _)
    override def encode(a: A): Http.Queries = query.encode(a) match
      case Query.Value.Some(value) => Vector(query.name -> value.some)
      case Query.Value.None        => Vector(query.name -> none)
      case Query.Value.Abscent     => Http.Queries.Empty
