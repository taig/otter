//package io.taig.crock.http
//
//import cats.{Invariant, InvariantSemigroupal}
//import cats.data.{Chain, Validated}
//import cats.syntax.all.*
//
//sealed abstract class Headers[A]:
//  def toChain: Chain[Header[?]]
//  def matches(headers: Http.Headers): Boolean
//  final def product[B](headers: Headers[B]): Headers[(A, B)] = Headers.Product(this, headers)
//  final transparent inline def zip[B](headers: Headers[B]): Headers[?] = inline (this, headers) match
//    case (left: Headers[Unit], right) => left.product(right).imap[B] { case (_, b) => b }(b => ((), b))
//    case (left, right: Headers[Unit]) => left.product(right).imap[A] { case (a, _) => a }(a => (a, ()))
//    case (left: Headers[? *: ?], right) =>
//      left.product(right).imap { case (a, b) => a :* b }(ab => (ab.init.asInstanceOf[A], ab.last.asInstanceOf[B]))
//    case (left, right) => left.product(right)
//  final transparent inline def :*[B](header: Header[B]): Headers[?] = zip(header.toHeaders)
//  final def imap[B](f: A => B)(g: B => A): Headers[B] = Headers.Modify(this, f, g)
//  final def decode(headers: Http.Headers): Validated[Violations, A] = decodeWithRemainders(headers).map(_._2)
//  def decodeWithRemainders(headers: Http.Headers): Validated[Violations, (Http.Headers, A)]
//  def encode(a: A): Http.Headers
//
//object Headers:
//  final private case class Root[A](header: Header[A]) extends Headers[A]:
//    override def toChain: Chain[Header[?]] = Chain.one(header)
//    override def matches(headers: Http.Headers): Boolean = headers.contains(header.name)
//    override def decodeWithRemainders(headers: Http.Headers): Validated[Violations, (Http.Headers, A)] =
//      header.decode(headers)
//    override def encode(a: A): Http.Headers = header.encode(a)
//
//  final private case class Product[A, B](left: Headers[A], right: Headers[B]) extends Headers[(A, B)]:
//    override def toChain: Chain[Header[?]] = left.toChain ++ right.toChain
//    override def matches(headers: Http.Headers): Boolean = left.matches(headers) && right.matches(headers)
//    override def decodeWithRemainders(headers: Http.Headers): Validated[Violations, (Http.Headers, (A, B))] =
//      left.decodeWithRemainders(headers) match
//        case Validated.Valid((remainders, a)) => right.decodeWithRemainders(remainders).map(_.tupleLeft(a))
//        case Validated.Invalid(violations)    => right.decode(headers).fold(violations merge _, _ => violations).invalid
//    override def encode(ab: (A, B)): Http.Headers = left.encode(ab._1) merge right.encode(ab._2)
//
//  final private case class Modify[A, B](
//      headers: Headers[A],
//      f: A => B,
//      g: B => A
//  ) extends Headers[B]:
//    export headers.{matches, toChain}
//    override def decodeWithRemainders(values: Http.Headers): Validated[Violations, (Http.Headers, B)] =
//      headers.decodeWithRemainders(values).map(_.map(f))
//    override def encode(b: B): Http.Headers = headers.encode(g(b))
//
//  val Empty: Headers[Unit] = new Headers[Unit]:
//    override def toChain: Chain[Header[?]] = Chain.empty
//    override def matches(headers: Http.Headers): Boolean = true
//    override def decodeWithRemainders(headers: Http.Headers): Validated[Violations, (Http.Headers, Unit)] =
//      (headers, ()).valid
//    override def encode(a: Unit): Http.Headers = Http.Headers.Empty
//
//  def apply[A](header: Header[A]): Headers[A] = Root(header)
//
//  given InvariantSemigroupal[Headers] with
//    override def imap[A, B](fa: Headers[A])(f: A => B)(g: B => A): Headers[B] = fa.imap(f)(g)
//    override def product[A, B](fa: Headers[A], fb: Headers[B]): Headers[(A, B)] = fa.product(fb)
