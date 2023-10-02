package io.taig.otter.http

import cats.data.{Chain, Validated}
import cats.syntax.all.*
import io.taig.otter.syntax.*
import io.taig.otter.validation.{Validation, Violations}
import io.taig.otter.{Collection, Value}

sealed abstract class Query[A](val name: String, val schema: Value[?] | Collection.Of[Value[?], ?]):
  self =>
  final def isOptional: Boolean = schema.isOptional

  final def isCollection: Boolean = schema match
    case _: Collection[?] => true
    case _                => false

  def ivalidate[B](validation: Validation[A, B])(g: B => A): Query[B]

  final def imap[B](f: A => B)(g: B => A): Query[B] = ivalidate(Validation.lift(f))(g)

  def optional: Query[Option[A]]

  def toQueries: Queries[A] = Queries(this)

  def matchesWithRemainders(queries: Http.Queries): Option[Http.Queries]

  def decodeWithRemainders(queries: Http.Queries): Validated[Violations, (Http.Queries, A)]

  def encode(a: A): Http.Queries

object Query:
  final private class Single[A](name: String, schema: Value[A]) extends Query[A](name, schema):
    override def ivalidate[B](validation: Validation[A, B])(g: B => A): Query[B] =
      new Single(name, schema.ivalidate(validation)(g))
    override def optional: Query[Option[A]] = new Single(name, schema.optional)
    override def matchesWithRemainders(queries: Http.Queries): Option[Http.Queries] =
      queries.firstWithRemainders(name).map(_._2)
    override def decodeWithRemainders(queries: Http.Queries): Validated[Violations, (Http.Queries, A)] =
      queries.firstWithRemainders(name) match
        case Some((head, tail)) => schema.parse(head.some).tupleLeft(tail)
        case None               => schema.parse(None).tupleLeft(queries)
    override def encode(a: A): Http.Queries = schema.print(a) match
      case value: String         => Chain.one(name -> value)
      case value: Option[String] => Chain.fromOption(value).tupleLeft(name)

  final private class Multiple[A](name: String, schema: Collection.Of[Value[?], A]) extends Query[A](name, schema):
    override def ivalidate[B](validation: Validation[A, B])(g: B => A): Query[B] =
      new Multiple(name, schema.ivalidate(validation)(g))

    override def optional: Query[Option[A]] = new Multiple(name, schema.optional)

    override def matchesWithRemainders(queries: Http.Queries): Option[Http.Queries] =
      queries.firstWithRemainders(name).map(_._2)

    override def decodeWithRemainders(queries: Http.Queries): Validated[Violations, (Http.Queries, A)] =
      val (head, tail) = queries.allWithRemainders(name)
      schema.parse(head.some).tupleLeft(tail)

    override def encode(a: A): Http.Queries = schema.print(a).getOrElse(Chain.empty).tupleLeft(name)

  def apply[A](name: String, schema: Value[A]): Query[A] = new Single[A](name, schema)
  def apply[A](name: String, schema: Collection.Of[Value[?], A]): Query[A] = new Multiple[A](name, schema)
