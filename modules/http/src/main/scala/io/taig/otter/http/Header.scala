//package io.taig.otter.http
//
//import cats.Eval
//import cats.data.{Chain, Validated}
//import cats.syntax.all.*
//import org.typelevel.ci.CIString
//
//// TODO default (?)
//sealed abstract class Header[A]:
//  def isOptional: Boolean
//  def name: CIString
//  def schema: Eval[Schema.Value[?] | Collection.Value[?]]
//  final def optional: Header[Option[A]] = Header.Optional(this)
//  final def imap[B](f: A => B)(g: B => A): Header[B] = Header.Modify(this, f, g)
//  final def toHeaders: Headers[A] = Headers(this)
//  def decode(headers: Http.Headers): Validated[Violations, (Http.Headers, A)]
//  def encode(a: A): Http.Headers
//
//object Header:
//  final private case class Single[A](name: CIString, schema: Eval[Schema.Value[A]]) extends Header[A]:
//    override def isOptional: Boolean = false
//    override def decode(headers: Http.Headers): Validated[Violations, (Http.Headers, A)] =
//      headers.getFirstWithRemainders(name) match
//        case Some((head, remainders)) => schema.value.parse(head).tupleLeft(remainders)
//        case None                     => Violations.rootNec(Constraint.required.toViolation(OpenApi.Null)).invalid
//    override def encode(a: A): Http.Headers = Http.Headers.one(name, schema.value.render(a))
//
//  final private case class Multiple[A](name: CIString, schema: Eval[Collection.Value[A]]) extends Header[A]:
//    override def isOptional: Boolean = false
//    override def decode(headers: Http.Headers): Validated[Violations, (Http.Headers, A)] =
//      headers.getWithRemainders(name) match
//        case Some((headers, remainders)) => schema.value.parse(headers.toChain.toVector).tupleLeft(remainders)
//        case None                        => schema.value.parse(Vector.empty).tupleLeft(headers)
//    override def encode(a: A): Http.Headers =
//      val values: Chain[String] = schema.value.encode(a).toChain.map(_.render)
//      Http.Headers(values.tupleLeft(name))
//
//  final private case class Optional[A](header: Header[A]) extends Header[Option[A]]:
//    export header.{name, schema}
//    override def isOptional: Boolean = true
//    override def decode(headers: Http.Headers): Validated[Violations, (Http.Headers, Option[A])] =
//      headers.getFirst(name) match
//        case Some(_) => header.decode(headers).map(_.map(_.some))
//        case None    => (headers, none[A]).valid
//    override def encode(a: Option[A]): Http.Headers = a.fold(Http.Headers.Empty)(header.encode)
//
//  final private case class Modify[A, B](header: Header[A], f: A => B, g: B => A) extends Header[B]:
//    export header.{isOptional, name, schema}
//    override def decode(headers: Http.Headers): Validated[Violations, (Http.Headers, B)] =
//      header.decode(headers).map(_.map(f))
//    override def encode(b: B): Http.Headers = header.encode(g(b))
//
//  def single[A](name: CIString, schema: Eval[Schema.Value[A]]): Header[A] = Single(name, schema)
//  def multiple[A](name: CIString, schema: Eval[Collection.Value[A]]): Header[A] = Multiple(name, schema)
