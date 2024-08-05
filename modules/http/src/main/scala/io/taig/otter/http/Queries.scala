package io.taig.otter.http

import cats.syntax.all.*
import io.taig.otter.Codec
import io.taig.otter.filterKeys
import io.taig.otter.Evidence

sealed abstract class Queries[A]:
  self =>

  def toVector: Vector[Query[?]]

  final def imap[B](f: A => B)(g: B => A): Queries[B] = new Queries[B]:
    export self.toVector
    override def decode(values: Http.Queries): Codec.Result[B] = self.decode(values).map(f)
    override def encode(b: B): Http.Queries = self.encode(g(b))

  final def zip[B](queries: Queries[B]): Queries[(A, B)] = new Queries[(A, B)]:
    override def toVector: Vector[Query[?]] = self.toVector ++ queries.toVector
    override def decode(values: Http.Queries): Codec.Result[(A, B)] =
      val (left, remainders) = values.filterKeys(self.toVector.map(_.name))
      val (right, _) = remainders.filterKeys(queries.toVector.map(_.name))
      (self.decode(left), queries.decode(right)).tupled
    override def encode(ab: (A, B)): Http.Queries = self.encode(ab._1) ++ queries.encode(ab._2)

  final def :*[B](query: Query[B])(using merge: Evidence.Merge[A, B]): Queries[merge.Out] =
    zip(query.toQueries).imap(merge.apply)(merge.unapply)

  final def *:[B](query: Query[B])(using merge: Evidence.Merge[B, A]): Queries[merge.Out] =
    query.toQueries.zip(this).imap(merge.apply)(merge.unapply)

  final def toUrl: Url[A] = Url(this)

  def decode(values: Http.Queries): Codec.Result[A]

  def encode(a: A): Http.Queries

object Queries:
  val Empty: Queries[Unit] = new Queries[Unit]:
    override def toVector: Vector[Query[?]] = Vector.empty
    override def decode(values: Http.Queries): Codec.Result[Unit] = ().valid
    override def encode(a: Unit): Http.Queries = Vector.empty

  def apply[A](query: Query[A]): Queries[A] = new Queries[A]:
    override def toVector: Vector[Query[?]] = Vector(query)
    override def decode(values: Http.Queries): Codec.Result[A] =
      query
        .decode(values.collectFirst { case (key, value) if key === query.name => value }.flatten)
        .leftMap(query.name /: _)
    override def encode(a: A): Http.Queries = Vector.from(query.encode(a).map(_.some).tupleLeft(query.name))
