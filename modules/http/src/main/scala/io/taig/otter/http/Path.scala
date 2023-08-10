package io.taig.otter.http

import cats.InvariantSemigroupal
import cats.data.{Chain, Validated}
import cats.syntax.all.*
import io.taig.otter.schema.Violations

sealed abstract class Path[A]:
  self =>
  def toChain: Chain[Segment[?]]

  final def imap[B](f: A => B)(g: B => A): Path[B] = new Path[B]:
    export self.toChain
    override def parseWithRemainders(remainders: Http.Path): Validated[Violations, (Http.Path, B)] =
      self.parseWithRemainders(remainders).map(_.map(f))
    override def print(b: B): Http.Path = self.print(g(b))

  final infix def zip[B](path: Path[B]): Path[(A, B)] = new Path[(A, B)]:
    override def toChain: Chain[Segment[?]] = self.toChain ++ path.toChain
    override def parseWithRemainders(remainders: Http.Path): Validated[Violations, (Http.Path, (A, B))] =
      self.parseWithRemainders(remainders).andThen { case (remainders, a) =>
        path.parseWithRemainders(remainders).map(_.tupleLeft(a))
      }
    override def print(ab: (A, B)): Http.Path = self.print(ab._1) ++ path.print(ab._2)

  def parseWithRemainders(remainders: Http.Path): Validated[Violations, (Http.Path, A)]
  def print(a: A): Http.Path

object Path extends ToPathOps:
  val Empty: Path[Unit] = new Path[Unit]:
    override def toChain: Chain[Segment[?]] = Chain.empty
    override def parseWithRemainders(remainders: Http.Path): Validated[Violations, (Http.Path, Unit)] =
      (remainders, ()).valid
    override def print(a: Unit): Http.Path = Chain.empty

  def apply[A](segment: Segment[A]): Path[A] = new Path[A]:
    override def toChain: Chain[Segment[?]] = Chain.one(segment)
    override def parseWithRemainders(remainders: Http.Path): Validated[Violations, (Http.Path, A)] = remainders.uncons
      .match
        case Some((head, tail)) if segment.isOptional =>
          segment.parse(head.some).tupleLeft(tail).findValid(segment.parse(none).tupleLeft(remainders))
        case Some((head, tail)) => segment.parse(head.some).tupleLeft(tail)
        case None               => segment.parse(none).tupleLeft(remainders)
      .leftMap(_.modifyHistory(segment.name /: _))
    override def print(a: A): Http.Path = Chain.fromOption(segment.print(a))

  given InvariantSemigroupal[Path] with
    override def imap[A, B](fa: Path[A])(f: A => B)(g: B => A): Path[B] = fa.imap(f)(g)
    override def product[A, B](fa: Path[A], fb: Path[B]): Path[(A, B)] = fa.zip(fb)
