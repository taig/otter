//package io.taig.otter.http
//
//import cats.data.{Chain, Validated}
//import cats.syntax.all.*
//import io.taig.otter.schema.{Collection, Schema, Violations}
//import io.taig.otter.http.syntax.*
//
//final case class Query[A](name: String, schema: Schema.Value[A] | Collection[Schema.Value, A]):
//  self =>
//  def isOptional: Boolean = schema.isOptional
//  def isCollection: Boolean = schema match
//    case _: Collection[?, ?] => true
//    case _                   => false
//
//  def imap[B](f: A => B)(g: B => A): Query[B] = schema match
//    case schema: Schema.Value[A]             => copy(schema = schema.imap(f)(g))
//    case schema: Collection[Schema.Value, A] => copy(schema = schema.imap(f)(g))
//
//  def optional: Query[Option[A]] = schema match
//    case schema: Schema.Value[A]             => copy(schema = schema.optional)
//    case schema: Collection[Schema.Value, A] => copy(schema = schema.optional)
//
//  def toQueries: Queries[A] = Queries(this)
//
//  def matchesWithRemainders(remainders: Http.Queries): Option[Http.Queries] = schema match
//    case _: Schema.Value[?]  => remainders.firstWithRemainders(name).map(_._2)
//    case _: Collection[?, ?] => remainders.removeAll(name).some
//
//  def decodeWithRemainders(remainders: Http.Queries): Validated[Violations, (Http.Queries, A)] = schema match
//    case schema: Schema.Value[A] =>
//      remainders.firstWithRemainders(name) match
//        case Some((head, tail)) => schema.parse(head.some).tupleLeft(tail)
//        case None               => schema.parse(none).tupleLeft(remainders)
//    case schema: Collection[Schema.Value, A] =>
//      val (head, tail) = remainders.allWithRemainders(name)
//      schema.parse(head.map(_.some).some).tupleLeft(tail)
//
//  def encode(a: A): Http.Queries = schema match
//    case schema: Schema.Value[A] => Chain.fromOption(schema.print(a)).tupleLeft(name)
//    case schema: Collection[Schema.Value, A] =>
//      Chain.fromOption(schema.print(a)).flatMap(_.mapFilter(identity)).tupleLeft(name)
