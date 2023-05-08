package io.taig.openapi.http

import cats.InvariantSemigroupal
import cats.data.{Chain, Validated}
import cats.syntax.all.*
import io.taig.openapi.{History, OpenApi}
import io.taig.openapi.syntax.*
import io.taig.openapi.schema.{Violations, Void}
import io.taig.openapi.validation.Constraint

sealed abstract class Path[A]:
  def toChain: Chain[Segment[?]]
  final def matches(path: Chain[String]): Boolean =
    val (remainders, matches) = matchesWithRemainders(path)
    matches && remainders.isEmpty
  def matchesWithRemainders(path: Chain[String]): (Chain[String], Boolean)
  final def product[B](path: Path[B]): Path[(A, B)] = Path.Product(this, path)
  final transparent inline def zip[B](path: Path[B]): Path[?] = inline (this, path) match
    case (left: Path[Void], right) => left.product(right).imap[B] { case (_, b) => b }(b => (Void, b))
    case (left, right: Path[Void]) => left.product(right).imap[A] { case (a, _) => a }(a => (a, Void))
    case (left: Path[? *: ?], right) =>
      left.product(right).imap { case (a, b) => a :* b }(ab => (ab.init.asInstanceOf[A], ab.last.asInstanceOf[B]))
    case (left, right) => left.product(right)
  final transparent inline def /[B](segment: Segment[B]): Path[?] = zip(segment.toPath)
  final transparent inline def /(name: String): Path[?] = /(Segment.Static(name))
  final def toUrl: Url[A] = Url(this)
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

  final private case class One[A](segment: Segment[A]) extends Path[A]:
    override def toChain: Chain[Segment[?]] = Chain.one(segment)
    override def matchesWithRemainders(path: Chain[String]): (Chain[String], Boolean) = path.uncons match
      case Some((head, tail)) => (tail, segment.matches(head))
      case None               => (path, false)
    override def decodeWithRemainders(path: Chain[String]): Validated[Violations, (Chain[String], A)] =
      path.uncons match
        case Some((head, tail)) => segment.decode(head).tupleLeft(tail)
        case None =>
          Violations
            .oneNec(History.Root / segment.name, Constraint.required.toViolation(OpenApi.Null))
            .invalid
    override def encode(a: A): Chain[String] = Chain.one(segment.encode(a))

  final private case class Product[A, B](left: Path[A], right: Path[B]) extends Path[(A, B)]:
    override def toChain: Chain[Segment[?]] = left.toChain ++ right.toChain
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
    override def toChain: Chain[Segment[?]] = path.toChain
    override def matchesWithRemainders(path: Chain[String]): (Chain[String], Boolean) =
      this.path.matchesWithRemainders(path)
    override def decodeWithRemainders(path: Chain[String]): Validated[Violations, (Chain[String], B)] =
      this.path.decodeWithRemainders(path).map(_.map(f))
    override def encode(b: B): Chain[String] = path.encode(g(b))

  val Root: Path[Void] = new Path[Void]:
    override def toChain: Chain[Segment[?]] = Chain.empty
    override def matchesWithRemainders(path: Chain[String]): (Chain[String], Boolean) = (path, true)
    override def decodeWithRemainders(path: Chain[String]): Validated[Violations, (Chain[String], Void)] =
      (path, Void).valid
    override def encode(a: Void): Chain[String] = Chain.empty

  def apply[A](segment: Segment[A]): Path[A] = One(segment)

  given InvariantSemigroupal[Path] with
    override def imap[A, B](fa: Path[A])(f: A => B)(g: B => A): Path[B] = fa.imap(f)(g)
    override def product[A, B](fa: Path[A], fb: Path[B]): Path[(A, B)] = fa.product(fb)
