package io.taig.openapi.http

import cats.data.{Chain, Validated}
import cats.syntax.all.*
import io.taig.openapi.{History, OpenApi}
import io.taig.openapi.syntax.*
import io.taig.openapi.schema.{Violations, Void}
import io.taig.openapi.validation.Constraint

sealed abstract class Path[A]:
  def segments: Chain[Segment[?]]
  final def matches(path: Chain[String]): Boolean =
    val (remainders, matches) = matchesWithRemainders(path)
    matches && remainders.isEmpty
  def matchesWithRemainders(path: Chain[String]): (Chain[String], Boolean)
  final def product[B](path: Path[B]): Path[(A, B)] = Path.Product(this, path)
  final def imap[B](f: A => B)(g: B => A): Path[B] = Path.Modify(this, f, g)
  final def decode(path: Chain[String]): Validated[Violations, A] =
    decodeWithRemainders(path).andThen { case (remainders, a) =>
      Validated.cond(
        remainders.isEmpty,
        a,
        Violations.rootNec(Constraint.text.equal("/".asOpenApi).toViolation(Path.printPath(remainders).asOpenApi))
      )
    }
  def decodeWithRemainders(path: Chain[String]): Validated[Violations, (Chain[String], A)]
  def encode(a: A): Chain[String]

object Path:
  private def printPath(path: Chain[String]): String = "/" + path.mkString_("/")

  final private case class Append[A, B](path: Path[A], segment: Segment[B]) extends Path[(A, B)]:
    override def segments: Chain[Segment[?]] = Chain.one(segment)
    override def matchesWithRemainders(path: Chain[String]): (Chain[String], Boolean) = path.initLast match
      case Some((head, tail)) =>
        val (remainders, result) = this.path.matchesWithRemainders(head)
        (remainders, result && segment.matches(tail))
      case None => (path, false)
    override def decodeWithRemainders(path: Chain[String]): Validated[Violations, (Chain[String], (A, B))] =
      this.path.decodeWithRemainders(path).andThen { case (remainders, a) =>
        remainders.uncons match
          case Some((head, tail)) => segment.decode(head).map(b => (tail, (a, b)))
          case None =>
            Violations.oneNec(History.Root / segment.name, Constraint.required.toViolation(OpenApi.Null)).invalid
      }
    override def encode(ab: (A, B)): Chain[String] = path.encode(ab._1) :+ segment.encode(ab._2)

  final private case class Product[A, B](left: Path[A], right: Path[B]) extends Path[(A, B)]:
    override def segments: Chain[Segment[?]] = left.segments ++ right.segments
    override def matchesWithRemainders(path: Chain[String]): (Chain[String], Boolean) =
      val (remainders1, result1) = left.matchesWithRemainders(path)
      val (remainders2, result2) = right.matchesWithRemainders(remainders1)
      (remainders2, result1 && result2)
    override def decodeWithRemainders(path: Chain[String]): Validated[Violations, (Chain[String], (A, B))] =
      left.decodeWithRemainders(path).andThen { case (remainders, a) =>
        right.decodeWithRemainders(remainders).map(_.tupleLeft(a))
      }
    override def encode(ab: (A, B)): Chain[String] = left.encode(ab._1) ++ right.encode(ab._2)

  final private case class Modify[A, B](path: Path[A], f: A => B, g: B => A) extends Path[B]:
    override def segments: Chain[Segment[?]] = path.segments
    override def matchesWithRemainders(path: Chain[String]): (Chain[String], Boolean) =
      this.path.matchesWithRemainders(path)
    override def decodeWithRemainders(path: Chain[String]): Validated[Violations, (Chain[String], B)] =
      this.path.decodeWithRemainders(path).map(_.map(f))
    override def encode(b: B): Chain[String] = path.encode(g(b))

  val Root: Path[Void] = new Path[Void]:
    override def segments: Chain[Segment[?]] = Chain.empty
    override def matchesWithRemainders(path: Chain[String]): (Chain[String], Boolean) = (path, true)
    override def decodeWithRemainders(path: Chain[String]): Validated[Violations, (Chain[String], Void)] =
      (path, Void).valid
    override def encode(a: Void): Chain[String] = Chain.empty
