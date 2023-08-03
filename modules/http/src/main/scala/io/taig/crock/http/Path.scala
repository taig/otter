package io.taig.crock.http

import cats.InvariantSemigroupal
import cats.data.Chain
import cats.syntax.all.*

sealed abstract class Path[A]:
  def toChain: Chain[Segment[?]]
  final infix def zip[B](path: Path[B]): Path[(A, B)] = Path.Zip(this, path)
  final infix def zip[B](segment: Segment[B]): Path[(A, B)] = zip(segment.toPath)
  final def /(name: String): Path[A] = zip(Segment.Static(name)).imap { case (a, _) => a }(a => (a, ()))
  final def imap[B](f: A => B)(g: B => A): Path[B] = Path.Modify(this, f, g)

object Path extends ToPathOps:
  private[crock] case object Empty extends Path[Unit]:
    override def toChain: Chain[Segment[?]] = Chain.empty

  final private[crock] case class One[A](segment: Segment[A]) extends Path[A]:
    override def toChain: Chain[Segment[?]] = Chain.one(segment)

  final private[crock] case class Zip[A, B](left: Path[A], right: Path[B]) extends Path[(A, B)]:
    override def toChain: Chain[Segment[?]] = left.toChain ++ right.toChain

  final private[crock] case class Modify[A, B](self: Path[A], f: A => B, g: B => A) extends Path[B]:
    override def toChain: Chain[Segment[?]] = self.toChain

  val Root: Path[Unit] = Empty
  def apply[A](segment: Segment[A]): Path[A] = One(segment)

  given InvariantSemigroupal[Path] with
    override def imap[A, B](fa: Path[A])(f: A => B)(g: B => A): Path[B] = fa.imap(f)(g)
    override def product[A, B](fa: Path[A], fb: Path[B]): Path[(A, B)] = fa.zip(fb)

// TODO prepend via /:
final class PathOps[A](self: Path[A]) extends AnyVal:
  inline def /(parameter: Segment[Unit]): Path[A] = self.zip(parameter).imap { case (a, _) => a }(a => (a, ()))
  inline def /[B](parameter: Segment[B]): Path[(A, B)] = self.zip(parameter)
final class PathOpsUnit(self: Path[Unit]) extends AnyVal:
  inline def /[B](parameter: Segment[B]): Path[B] = self.zip(parameter).imap { case (_, b) => b }(((), _))
final class PathOpsTuple[A <: Tuple](self: Path[A]) extends AnyVal:
  inline def /(parameter: Segment[Unit]): Path[A] = self.zip(parameter).imap { case (a, _) => a }((_, ()))
  inline def /[B](parameter: Segment[B]): Path[Tuple.Append[A, B]] =
    self.zip(parameter).imap { case (a, b) => a :* b }(ab => (ab.init.asInstanceOf[A], ab.last.asInstanceOf[B]))

trait ToPathOps extends ToPathOps1:
  implicit final def toPathOpsUnit(self: Path[Unit]): PathOpsUnit = PathOpsUnit(self)
  implicit final def toPathOpsTuple[A <: Tuple](self: Path[A]): PathOpsTuple[A] = PathOpsTuple(self)
trait ToPathOps1:
  implicit final def toPathOps[A](self: Path[A]): PathOps[A] = PathOps(self)
