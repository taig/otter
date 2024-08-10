package io.taig.otter.http

import cats.syntax.all.*
import cats.Invariant
import io.taig.otter.Codec
import io.taig.otter.Violations
import io.taig.otter.Violation
import io.taig.otter.Constraint
import io.taig.otter.Data
import java.util.regex.Pattern
import cats.data.Validated
import io.taig.otter.Evidence

sealed abstract class Path[A]:
  self =>

  def toVector: Vector[Segment[?]]

  def matches(path: Http.Path): Boolean

  final def imap[B](f: A => B)(g: B => A): Path[B] = new Path[B]:
    export self.{matches, toVector}
    override def decode(values: Http.Path): Codec.Result[B] = self.decode(values).map(f)
    override def encode(b: B): Http.Path = self.encode(g(b))

  final def zip[B](path: Path[B]): Path[(A, B)] = new Path[(A, B)]:
    override def toVector: Vector[Segment[?]] = self.toVector ++ path.toVector
    override def matches(value: Http.Path): Boolean =
      val (left, right) = value.splitAt(self.toVector.length)
      self.matches(left) && path.matches(right)
    override def decode(values: Http.Path): Codec.Result[(A, B)] =
      val (left, right) = values.splitAt(self.toVector.length)
      (self.decode(left), path.decode(right)).tupled
    override def encode(ab: (A, B)): Http.Path = self.encode(ab._1) ++ path.encode(ab._2)

  final def /(segment: String): Path[A] = zip(Segment.Static(segment).toPath).imap { case (a, _) => a }(a => (a, ()))

  final def /[B](segment: Segment.Parameter[B])(using merge: Evidence.Merge[A, B]): Path[merge.Out] =
    zip(segment.toPath).imap(merge.apply)(merge.unapply)

  final def toUrl: Url[A] = Url(this)

  def decode(values: Http.Path): Codec.Result[A]

  def encode(a: A): Http.Path

object Path:
  val Empty: Path[Unit] = new Path[Unit]:
    override def toVector: Vector[Segment[?]] = Vector.empty
    override def matches(path: Http.Path): Boolean = path.isEmpty
    override def decode(values: Http.Path): Codec.Result[Unit] = Validated.cond(
      values.isEmpty,
      (),
      Violations.rootNec(
        Violation(
          Constraint.Primitive.Matches(Pattern.compile(Pattern.quote("/"))),
          actual = Data.String("/" + values.mkString("/"))
        )
      )
    )
    override def encode(a: Unit): Http.Path = Vector.empty

  def apply[A](segment: Segment[A]): Path[A] = new Path[A]:
    override def toVector: Vector[Segment[?]] = Vector(segment)
    override def matches(path: Http.Path): Boolean = path match
      case Vector(value) => segment.matches(value)
      case _ => false
    override def decode(values: Http.Path): Codec.Result[A] = values match
      case Vector(value) => segment.decode(value)
      case Vector() =>
        Violations
          .rootNec(
            Violation(
              Constraint.Primitive.Matches(Pattern.compile(Pattern.quote(s"/${segment.print}"))),
              actual = Data.String("/")
            )
          )
          .invalid
      case _ =>
        Violations
          .rootNec(
            Violation(
              Constraint.Primitive.Matches(Pattern.compile(Pattern.quote(s"/${segment.print}"))),
              actual = Data.String("/" + values.mkString("/"))
            )
          )
          .invalid
    override def encode(a: A): Http.Path = Vector(segment.encode(a))

  given Invariant[Path] with
    override def imap[A, B](fa: Path[A])(f: A => B)(g: B => A): Path[B] = fa.imap(f)(g)
