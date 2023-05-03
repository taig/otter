package io.taig.openapi.http

import cats.data.{Chain, Validated}
import cats.syntax.all.*
import io.taig.openapi.History
import io.taig.openapi.syntax.*
import io.taig.openapi.schema.{Violations, Void}
import io.taig.openapi.validation.Constraint

sealed abstract class Path[A]:
  def segments: Chain[Segment[?]]
  final def matches(path: Chain[String]): Boolean =
    val (remainders, matches) = matchesWithRemainders(path)
    matches && remainders.isEmpty
  def matchesWithRemainders(path: Chain[String]): (Chain[String], Boolean)
  def decode(path: Chain[String]): Validated[Violations, A]
  def encode(a: A): Chain[String]

object Path:
  private def printPath(path: Chain[String]): String = "/" + path.mkString_("/")

  final private case class Append[A, B](path: Path[A], segment: Segment[B]) extends Path[(A, B)]:
    override def segments: Chain[Segment[?]] = Chain.one(segment)
    override def matchesWithRemainders(path: Chain[String]): (Chain[String], Boolean) =
      path.initLast match
        case Some((head, tail)) =>
          val (remainders, result) = this.path.matchesWithRemainders(head)
          (remainders, result && segment.matches(tail))
        case None => (path, false)
    override def decode(path: Chain[String]): Validated[Violations, (A, B)] = path.initLast match
      case Some((head, tail)) =>
        this.path.decode(head).andThen { a =>
          segment.decode(tail).tupleLeft(a)
        }
      case None => ???
//      this.path.decodeWithRemainders(path).andThen { case (remainders, a) =>
//        remainders.uncons match
//          case Some((head, tail)) => segment.decode(head).map(b => (tail, (a, b)))
//          case None =>
//            Violations
//              .oneNec(
//                History.Root / segment.name,
//                Constraint.text.equal("/".asOpenApi).toViolation(s"/${segment.print}".asOpenApi)
//              )
//              .invalid
//      }
    override def encode(ab: (A, B)): Chain[String] = path.encode(ab._1) :+ segment.encode(ab._2)

  final private case class Product[A, B](left: Path[A], right: Path[B]) extends Path[(A, B)]:
    override def segments: Chain[Segment[?]] = left.segments ++ right.segments
    override def matchesWithRemainders(path: Chain[String]): (Chain[String], Boolean) = ???
    override def decode(path: Chain[String]): Validated[Violations, (A, B)] = ???
    override def encode(ab: (A, B)): Chain[String] = left.encode(ab._1) ++ right.encode(ab._2)

  final private case class Modify[A, B](path: Path[A], f: A => B, g: B => A) extends Path[B]:
    override def segments: Chain[Segment[?]] = path.segments
    override def matchesWithRemainders(path: Chain[String]): (Chain[String], Boolean) =
      this.path.matchesWithRemainders(path)
    override def decode(path: Chain[String]): Validated[Violations, B] = this.path.decode(path).map(f)
    override def encode(b: B): Chain[String] = path.encode(g(b))

  val Root: Path[Void] = new Path[Void]:
    override def segments: Chain[Segment[?]] = Chain.empty
    override def matchesWithRemainders(path: Chain[String]): (Chain[String], Boolean) = (path, true)
    override def decode(path: Chain[String]): Validated[Violations, Void] =
      Validated.cond(
        matches(path),
        Void,
        Violations.rootNec(Constraint.text.equal("/".asOpenApi).toViolation(printPath(path).asOpenApi))
      )
    override def encode(a: Void): Chain[String] = Chain.empty
