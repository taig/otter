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
    override def decodeWithRemainders(remainders: Http.Path): Validated[Violations, (Http.Path, B)] =
      self.decodeWithRemainders(remainders).map(_.map(f))
    override def encode(b: B): Http.Path = self.encode(g(b))

  final infix def zip[B](path: Path[B]): Path[(A, B)] = ???

  def decodeWithRemainders(remainders: Http.Path): Validated[Violations, (Http.Path, A)]
  def encode(a: A): Http.Path

object Path extends ToPathOps:
  val Empty: Path[Unit] = new Path[Unit]:
    override def toChain: Chain[Segment[?]] = Chain.empty
    override def decodeWithRemainders(remainders: Http.Path): Validated[Violations, (Http.Path, Unit)] =
      (remainders, ()).valid
    override def encode(a: Unit): Http.Path = Chain.empty

  def apply[A](segment: Segment[A]): Path[A] = new Path[A]:
    override def toChain: Chain[Segment[?]] = Chain.one(segment)
    override def decodeWithRemainders(remainders: Http.Path): Validated[Violations, (Http.Path, A)] = ???
    override def encode(a: A): Http.Path = Chain.fromOption(segment.encode(a))

  given InvariantSemigroupal[Path] with
    override def imap[A, B](fa: Path[A])(f: A => B)(g: B => A): Path[B] = fa.imap(f)(g)
    override def product[A, B](fa: Path[A], fb: Path[B]): Path[(A, B)] = fa.zip(fb)
