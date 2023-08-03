package io.taig.crock.http

import cats.InvariantSemigroupal
import cats.data.{Chain, Validated}
import cats.syntax.all.*

sealed abstract class Url[A]:
  def path: Path[?]
  def queries: Queries[?]

object Url:
  private[crock] object Empty extends Url[Unit]:
    override def path: Path[?] = Path.Root
    override def queries: Queries[?] = Queries.Root

  final private[crock] case class FromPath[A](path: Path[A]) extends Url[A]:
    override def queries: Queries[?] = Queries.Root

  final private[crock] case class FromQueries[A](queries: Queries[A]) extends Url[A]:
    override def path: Path[?] = Path.Root

  final private[crock] case class Zip[A, B](left: Url[A], right: Url[B]) extends Url[(A, B)]:
    override def path: Path[?] = left.path.product(right.path)
    override def queries: Queries[?] = left.queries.product(right.queries)

  final private[crock] case class Modify[A, B](self: Url[A], f: A => B, g: B => A) extends Url[B]:
    override def path: Path[?] = self.path
    override def queries: Queries[?] = self.queries

  val Root: Url[Unit] = Empty

  def apply[A](path: Path[A]): Url[A] = FromPath(path)
  def apply[A](queries: Queries[A]): Url[A] = FromQueries(queries)

  given InvariantSemigroupal[Url] with
    override def imap[A, B](fa: Url[A])(f: A => B)(g: B => A): Url[B] = fa.imap(f)(g)
    override def product[A, B](fa: Url[A], fb: Url[B]): Url[(A, B)] = fa.product(fb)
