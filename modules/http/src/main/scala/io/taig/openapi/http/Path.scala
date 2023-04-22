package io.taig.openapi.http

import cats.data.{Chain, Validated}
import cats.syntax.all.*
import io.taig.openapi.{Encoder, OpenApi}
import io.taig.openapi.schema.{andThenValidate, Evidence, InvariantValidation, Violations}
import io.taig.screening.{identifiers, Validation}
import io.taig.screening.syntax.*

import scala.Tuple.Append

abstract class Path[A](val segments: Chain[Segment[?]]):
  self =>

  final def imap[B](f: A => B)(g: B => A): Path[B] = new Path[B](segments) {
    override def decodeWithRemainders(
        values: Chain[OpenApi.Primitive]
    ): Validated[Violations, (Chain[OpenApi.Primitive], B)] =
      self.decodeWithRemainders(values).map(_.map(f))
    override def encode(b: B): Chain[OpenApi.Primitive] = self.encode(g(b))
    override def print: Chain[String] = self.print
  }
  final def gimap[B](using evidence: Evidence.Product.Aux[B, A]): Path[B] = imap(evidence.from)(evidence.to)
  final def ivalidate[B: Encoder, C](validation: Validation[B, A, A, C])(g: C => A): Path[C] = new Path[C](segments):
    override def decodeWithRemainders(
        values: Chain[OpenApi.Primitive]
    ): Validated[Violations, (Chain[OpenApi.Primitive], C)] = self
      .decodeWithRemainders(values)
      .andThen(_.traverse(andThenValidate(validation, a => OpenApi.fromChain(self.encode(a)))))
    override def encode(c: C): Chain[OpenApi.Primitive] = self.encode(g(c))
    override def print: Chain[String] = self.print

  final def zip[B](path: Path[B]): Path[(A, B)] = new Path[(A, B)](self.segments ++ path.segments):
    override def print: Chain[String] = self.print ++ path.print

    override def decodeWithRemainders(
        values: Chain[OpenApi.Primitive]
    ): Validated[Violations, (Chain[OpenApi.Primitive], (A, B))] =
      self.decodeWithRemainders(values).andThen { case (remainders, a) =>
        path.decodeWithRemainders(remainders).map { case (remainders, b) => (remainders, (a, b)) }
      }

    override def encode(ab: (A, B)): Chain[OpenApi.Primitive] = self.encode(ab._1) ++ path.encode(ab._2)

  final def zipQueries[B](queries: Queries[B]): Url[(A, B)] = Url(this, queries)

  final def :/[B](segment: Segment[B]): Path[(A, B)] = zip(segment.toPath)

  final def :?[B](query: Query[B]): Url[(A, B)] = zipQueries(query.toQueries)

  final def matches(path: Chain[OpenApi.Primitive]): Boolean = if path.length =!= segments.length then false
  else
    path.zipWith(segments)((path, segment) => (path, segment)).forall {
      case (path, Segment.Value(value)) => path.print === value
      case (_, _: Segment.Parameter[?]) => true
    }

  final def decode(values: Chain[OpenApi.Primitive]): Validated[Violations, A] = decodeWithRemainders(values).map(_._2)

  def decodeWithRemainders(values: Chain[OpenApi.Primitive]): Validated[Violations, (Chain[OpenApi.Primitive], A)]

  def encode(a: A): Chain[OpenApi.Primitive]

  def print: Chain[String]

  final def render(a: A): Chain[String] = encode(a).map(_.print)

  final def toUrl: Url[A] = Url.fromPath(this)

object Path:
  val Root: Path[Unit] = new Path[Unit](Chain.empty):
    override def print: Chain[String] = Chain.empty

    override def decodeWithRemainders(
        values: Chain[OpenApi.Primitive]
    ): Validated[Violations, (Chain[OpenApi.Primitive], Unit)] = (values, ()).valid

    override def encode(a: Unit): Chain[OpenApi.Primitive] = Chain.empty

  def one[A](segment: Segment[A]): Path[A] = new Path[A](Chain.one(segment)):
    override def print: Chain[String] = Chain.one(segment.print)

    override def decodeWithRemainders(
        values: Chain[OpenApi.Primitive]
    ): Validated[Violations, (Chain[OpenApi.Primitive], A)] = values.uncons match
      case Some((head, tail)) => segment.decode(head).tupleLeft(tail)
      case None =>
        Violations.rootNec {
          identifiers.collection.nonEmpty
            .toConstraint(reference = OpenApi.fromString(segment.print).some)
            .toViolation(actual = OpenApi.Array.Empty)
        }.invalid

    override def encode(a: A): Chain[OpenApi.Primitive] = Chain.one(segment.encode(a))

  given InvariantValidation[Path] with
    override def imap[A, B](fa: Path[A])(f: A => B)(g: B => A): Path[B] = fa.imap(f)(g)
    override def ivalidate[A: Encoder, B, C](fa: Path[B])(validation: Validation[A, B, B, C])(g: C => B): Path[C] =
      fa.ivalidate(validation)(g)
