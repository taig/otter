package io.taig.otter.http

import cats.data.{Chain, Validated}
import cats.syntax.all.*
import io.taig.otter.http.syntax.*
import io.taig.otter.validation.Violations
import io.taig.otter.{Schema, StringDecoder, StringEncoder}

final case class Query[A](name: String, schema: Schema.Value[A] | Schema.Collection[Schema.Value, A]):
  self =>
  def isOptional: Boolean = schema.isOptional
  def isCollection: Boolean = schema match
    case _: Schema.Collection[?, ?] => true
    case _                          => false

  def imap[B](f: A => B)(g: B => A): Query[B] = schema match
    case schema: Schema.Value[A]                    => copy(schema = schema.imap(f)(g))
    case schema: Schema.Collection[Schema.Value, A] => copy(schema = schema.imap(f)(g))

  def optional: Query[Option[A]] = schema match
    case schema: Schema.Value[A]                    => copy(schema = schema.optional)
    case schema: Schema.Collection[Schema.Value, A] => copy(schema = schema.optional)

  def toQueries: Queries[A] = Queries(this)

  def matchesWithRemainders(remainders: Http.Queries): Option[Http.Queries] = schema match
    case _: Schema.Value[?]         => remainders.firstWithRemainders(name).map(_._2)
    case _: Schema.Collection[?, ?] => remainders.removeAll(name).some

  def decodeWithRemainders(remainders: Http.Queries): Validated[Violations, (Http.Queries, A)] = schema match
    case schema: Schema.Value[A] =>
      remainders.firstWithRemainders(name) match
        case Some((head, tail)) => StringDecoder.value.decode(schema, head.some).tupleLeft(tail)
        case None               => StringDecoder.value.decode(schema, none).tupleLeft(remainders)
    case schema: Schema.Collection[Schema.Value, A] =>
      val (head, tail) = remainders.allWithRemainders(name)
      StringDecoder.collection.decode(schema, head.some).tupleLeft(tail)

  def encode(a: A): Http.Queries = schema match
    case schema: Schema.Value[A] => Chain.fromOption(StringEncoder.value.encode(schema, a)).tupleLeft(name)
    case schema: Schema.Collection[Schema.Value, A] =>
      StringEncoder.collection.encode(schema, a).getOrElse(Chain.empty).tupleLeft(name)
