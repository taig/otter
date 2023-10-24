package io.taig.otter.http

import cats.data.{Chain, Validated}
import cats.syntax.all.*
import io.taig.otter.Collection.Of
import io.taig.otter.syntax.*
import io.taig.otter.validation.Violations
import io.taig.otter.{Collection, Evidence, Value}

sealed abstract class Query[A](val explode: Boolean, val name: String, val style: Query.Style):
  self =>
  final def isOptional: Boolean = codec.isOptional

  final def isCollection: Boolean = codec match
    case _: Collection[?] => true
    case _                => false

  def codec: Value[A] | Collection.Of[Value[?], A]

  final def explode(f: Boolean => Boolean): Query[A] = new Query[A](f(explode), name, style) { export self.* }
  final def explode(value: Boolean): Query[A] = explode(_ => value)

  final def style(f: Query.Style => Query.Style): Query[A] = new Query[A](explode, name, f(style)) { export self.* }
  final def style(value: Query.Style): Query[A] = style(_ => value)

  final def +?[B](queries: Queries[B])(using evidence: Evidence.Merge[A, B]): Queries[evidence.Out] =
    toQueries +? queries

  final def +?[B](query: Query[B])(using evidence: Evidence.Merge[A, B]): Queries[evidence.Out] = toQueries +? query

  def toQueries: Queries[A] = Queries(this)

  final def matchesWithRemainders(queries: Http.Queries): Option[Http.Queries] =
    matchesWithRemainders(queries, isOptional)
  protected def matchesWithRemainders(queries: Http.Queries, optional: Boolean): Option[Http.Queries]

  final def decodeWithRemainders(queries: Http.Queries): Validated[Violations, (Http.Queries, A)] =
    decodeWithRemainders(queries, explode, style)
  protected def decodeWithRemainders(
      queries: Http.Queries,
      explode: Boolean,
      style: Query.Style
  ): Validated[Violations, (Http.Queries, A)]

  final def encode(a: A): Http.Queries = encode(a, explode, style)
  protected def encode(a: A, explode: Boolean, style: Query.Style): Http.Queries

object Query:
  enum Style:
    case Form
    case SpaceDelimited
    case PipeDelimited
    case DeepObject

  object Style:
    val Default: Query.Style = Form

  def apply[A](name: String, of: Value[A]): Query[A] = new Query[A](true, name, Style.Default):
    override def codec: Value[A] = of
    override def matchesWithRemainders(queries: Http.Queries, optional: Boolean): Option[Http.Queries] =
      queries.firstWithRemainders(this.name) match
        case Some((_, tail)) => Some(tail)
        case None            => Option.when(optional)(queries)
    override def decodeWithRemainders(
        queries: Http.Queries,
        explode: Boolean,
        style: Style
    ): Validated[Violations, (Http.Queries, A)] = queries.firstWithRemainders(this.name) match
      case Some((head, tail)) => of.parse(head.some).tupleLeft(tail)
      case None               => of.parse(None).tupleLeft(queries)
    override def encode(a: A, explode: Boolean, style: Style): Http.Queries = of.print(a) match
      case value: String         => Chain.one(this.name -> value)
      case value: Option[String] => Chain.fromOption(value).tupleLeft(this.name)

  def apply[A](name: String, of: Collection.Of[Value[?], A]): Query[A] = new Query[A](true, name, Style.Default):
    override def codec: Collection.Of[Value[?], A] = of
    override def matchesWithRemainders(queries: Http.Queries, optional: Boolean): Option[Http.Queries] =
      val (head, tail) = queries.allWithRemainders(this.name)
      if optional then Some(tail)
      else if head.isEmpty then None
      else Some(tail)
    override def decodeWithRemainders(
        queries: Http.Queries,
        explode: Boolean,
        style: Style
    ): Validated[Violations, (Http.Queries, A)] = (explode, style) match
      case (true, _) =>
        val (head, tail) = queries.allWithRemainders(this.name)
        codec.parse(head.some).tupleLeft(tail)
      case (false, Query.Style.Form) =>
        queries.firstWithRemainders(this.name) match
          case Some((head, tail)) =>
            codec.parse(Chain.fromIterableOnce(head.split(',')).some).tupleLeft(tail)
          case None => codec.parse(None).tupleLeft(queries)
      case (_, style) => throw new NotImplementedError(s"Query style $style is not supported yet")
    override def encode(a: A, explode: Boolean, style: Style): Http.Queries = (explode, style) match
      case (true, _) => of.print(a).getOrElse(Chain.empty).tupleLeft(this.name)
      case (false, Query.Style.Form) =>
        val values = of.print(a).getOrElse(Chain.empty)
        if values.isEmpty then Chain.empty else Chain.one(this.name -> values.mkString_(","))
      case (_, style) => throw new NotImplementedError(s"Query style $style is not supported yet")
