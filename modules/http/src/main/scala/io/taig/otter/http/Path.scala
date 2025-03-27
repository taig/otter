package io.taig.otter.http

import cats.Invariant
import cats.Show
import cats.data.Validated
import cats.syntax.all.*
import io.taig.otter.Codec
import io.taig.otter.Constraint
import io.taig.otter.Convert
import io.taig.otter.Data
import io.taig.otter.Merge
import io.taig.otter.Violation
import io.taig.otter.Violations

import java.util.regex.Pattern

sealed abstract class Path[A]:
  self =>

  def toVector: Vector[Segment[?]]

  def matches(path: Http.Path): Boolean

  final def imap[B](f: A => B)(g: B => A): Path[B] = new Path[B]:
    export self.{matches, toVector}
    override def decodeWithRemainders(values: Http.Path): Codec.Result[(Http.Path, B)] =
      self.decodeWithRemainders(values).map(_.map(f))
    override def encode(b: B): Http.Path = self.encode(g(b))

  final def to[B](using convert: Convert[A, B]): Path[B] = imap(convert.to)(convert.from)

  final def zip[B](path: Path[B]): Path[(A, B)] = new Path[(A, B)]:
    override def toVector: Vector[Segment[?]] = self.toVector ++ path.toVector
    override def matches(value: Http.Path): Boolean =
      val (left, right) = value.splitAt(self.toVector.length)
      self.matches(left) && path.matches(right)
    override def decodeWithRemainders(values: Http.Path): Codec.Result[(Http.Path, (A, B))] =
      self
        .decodeWithRemainders(values)
        .andThen:
          case (values, a) => path.decodeWithRemainders(values).map(_.tupleLeft(a))
    override def encode(ab: (A, B)): Http.Path = self.encode(ab._1) ++ path.encode(ab._2)

  final def /(segment: String): Path[A] = zip(Segment.Static(segment).toPath).imap { case (a, _) => a }(a => (a, ()))

  final def /[B](segment: Segment.Parameter[B])(using merge: Merge[A, B]): Path[merge.Out] =
    zip(segment.toPath).imap(merge.apply)(merge.unapply)

  final def toUrl: Url[A] = Url(this)

  final def decode(values: Http.Path): Codec.Result[A] = decodeWithRemainders(values).andThen:
    case (values, a) =>
      Validated.cond(
        values.isEmpty,
        a,
        Violations.rootNec(
          Violation(
            Constraint.Primitive.Matches(Pattern.compile(Pattern.quote("/"))),
            actual = String("/" + values.mkString("/"))
          )
        )
      )

  protected[otter] def decodeWithRemainders(values: Http.Path): Codec.Result[(Http.Path, A)]

  def encode(a: A): Http.Path

object Path:
  val Empty: Path[Unit] = new Path[Unit]:
    override def toVector: Vector[Segment[?]] = Vector.empty
    override def matches(path: Http.Path): Boolean = path.isEmpty
    override def decodeWithRemainders(values: Http.Path): Codec.Result[(Http.Path, Unit)] =
      (values, ()).valid
    override def encode(a: Unit): Http.Path = Vector.empty

  def apply[A](segment: Segment[A]): Path[A] = new Path[A]:
    override def toVector: Vector[Segment[?]] = Vector(segment)
    override def matches(path: Http.Path): Boolean = path match
      case Vector(value) => segment.matches(value)
      case _             => false
    override def decodeWithRemainders(values: Http.Path): Codec.Result[(Http.Path, A)] =
      values.headOption match
        case Some(value) => segment.decode(value).tupleLeft(values.tail)
        case None =>
          Violations
            .rootNec(
              Violation(
                Constraint.Primitive.Matches(Pattern.compile(Pattern.quote(show"/$segment"))),
                actual = String("/")
              )
            )
            .invalid
    override def encode(a: A): Http.Path = Vector(segment.encode(a))

  given Invariant[Path] with
    override def imap[A, B](fa: Path[A])(f: A => B)(g: B => A): Path[B] = fa.imap(f)(g)

  given Show[Path[?]] = path => "/" + path.toVector.map(_.show).mkString("/")
