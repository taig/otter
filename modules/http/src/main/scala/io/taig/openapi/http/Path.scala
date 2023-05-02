package io.taig.openapi.http

import cats.data.{Chain, Validated}
import cats.syntax.all.*
import io.taig.openapi.schema.{Violations, Void}

sealed abstract class Path[A]:
  def segments: Chain[Segment[?]]
  def matches(path: Chain[String]): Boolean
  final def decode(path: Chain[String]): Validated[Violations, A] = decodeWithRemainders(path).map(_._2)
  def decodeWithRemainders(path: Chain[String]): Validated[Violations, (Chain[String], A)]
  def encode(a: A): Chain[String]

object Path:
  final private case class One[A](segment: Segment[A]) extends Path[A]:
    override def segments: Chain[Segment[?]] = Chain.one(segment)
    override def matches(path: Chain[String]): Boolean = segment.matches(???)
    override def decodeWithRemainders(path: Chain[String]): Validated[Violations, (Chain[String], A)] =
      path.initLast match
        case Some((init, last)) => segment.decode(last).tupleLeft(init)
        case None               => ???
    override def encode(a: A): Chain[String] = Chain.one(segment.encode(a))

  final private case class Product[A, B](left: Path[A], right: Path[B]) extends Path[(A, B)]:
    override def segments: Chain[Segment[?]] = left.segments ++ right.segments
    override def matches(path: Chain[String]): Boolean = ???
    override def decodeWithRemainders(path: Chain[String]): Validated[Violations, (Chain[String], (A, B))] = ???
    override def encode(ab: (A, B)): Chain[String] = left.encode(ab._1) ++ right.encode(ab._2)

  final private case class Modify[A, B](path: Path[A], f: A => B, g: B => A) extends Path[B]:
    override def segments: Chain[Segment[?]] = path.segments
    override def matches(path: Chain[String]): Boolean = this.path.matches(path)
    override def decodeWithRemainders(path: Chain[String]): Validated[Violations, (Chain[String], B)] =
      this.path.decodeWithRemainders(path).map(_.map(f))
    override def encode(b: B): Chain[String] = path.encode(g(b))

  val Root: Path[Void] = new Path[Void]:
    override def segments: Chain[Segment[?]] = Chain.empty
    override def matches(path: Chain[String]): Boolean = path.isEmpty
    override def decodeWithRemainders(path: Chain[String]): Validated[Violations, (Chain[String], Void)] =
      Validated.cond(matches(path), (path, Void), ???)
    override def encode(a: Void): Chain[String] = Chain.empty
