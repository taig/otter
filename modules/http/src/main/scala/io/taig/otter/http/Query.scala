package io.taig.otter.http

import cats.data.{Chain, Validated}
import cats.syntax.all.*
import io.taig.otter.schema.{Collection, Schema, Violations}
import io.taig.otter.http.syntax.*

sealed abstract class Query[A]:
  self =>
  def name: String
  def schema: Schema.Value[?] | Collection[Schema.Value, ?]
  final def isOptional: Boolean = schema.isOptional
  final def isCollection: Boolean = schema match
    case _: Collection[?, ?] => true
    case _                   => false

  final def imap[B](f: A => B)(g: B => A): Query[B] = new Query[B]:
    export self.{matchesWithRemainders, name, schema}
    override def decodeWithRemainders(remainders: Http.Queries): Validated[Violations, (Http.Queries, B)] =
      self.decodeWithRemainders(remainders).map(_.map(f))
    override def encode(b: B): Http.Queries = self.encode(g(b))

  final def toQueries: Queries[A] = Queries(this)

  def matchesWithRemainders(remainders: Http.Queries): Option[Http.Queries]

  def decodeWithRemainders(remainders: Http.Queries): Validated[Violations, (Http.Queries, A)]
  def encode(a: A): Http.Queries

object Query:
  def apply[A](value: String, of: Schema.Value[A]): Query[A] = new Query[A]:
    override def name: String = value
    override def schema: Schema.Value[A] = of
    override def matchesWithRemainders(remainders: Http.Queries): Option[Http.Queries] =
      remainders.firstWithRemainders(name).map(_._2)
    override def decodeWithRemainders(remainders: Http.Queries): Validated[Violations, (Http.Queries, A)] =
      remainders.firstWithRemainders(name) match
        case Some((head, tail)) => schema.parse(head.some).tupleLeft(tail)
        case None               => schema.parse(none).tupleLeft(remainders)
    override def encode(a: A): Http.Queries = Chain.fromOption(schema.print(a)).tupleLeft(name)

  def apply[A](collection: String, of: Collection[Schema.Value, A]): Query[A] = new Query[A]:
    override def name: String = collection
    override def schema: Collection[Schema.Value, A] = of
    override def matchesWithRemainders(remainders: Http.Queries): Option[Http.Queries] = remainders.removeAll(name).some
    override def decodeWithRemainders(remainders: Http.Queries): Validated[Violations, (Http.Queries, A)] =
      val (head, tail) = remainders.allWithRemainders(name)
      schema.parse(head.map(_.some).some).tupleLeft(tail)
    override def encode(a: A): Http.Queries =
      Chain.fromOption(schema.print(a)).flatMap(_.mapFilter(identity)).tupleLeft(name)
