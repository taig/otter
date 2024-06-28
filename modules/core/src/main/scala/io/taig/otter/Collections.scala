package io.taig.otter

import scala.collection.immutable.SortedSet
import cats.implicits.*
import cats.Order
import cats.data.NonEmptyList
import cats.data.NonEmptySeq
import cats.data.NonEmptySet

trait Collections extends Validations:
  def vector[A]: Transformation.Plain[Vector[A], Vector[A]] = Transformation.ask

  def seq[A]: Transformation.Plain[Vector[A], Seq[A]] = vector[A].imap(_.toSeq)(_.toVector)

  def nonEmptySeq[A]: Transformation[Vector[A], Constraint.Collection, Long, NonEmptySeq[A]] =
    seq[A].ivalidate(nonEmpty)(_ +: _).imap(NonEmptySeq.apply)(fa => (fa.head, fa.tail))

  def list[A]: Transformation.Plain[Vector[A], List[A]] = vector[A].imap(_.toList)(_.toVector)

  def nonEmptyList[A]: Transformation[Vector[A], Constraint.Collection, Long, NonEmptyList[A]] =
    list[A].ivalidate(nonEmpty)(_ :: _).imap(NonEmptyList.apply)(fa => (fa.head, fa.tail))

  def set[A: Order]: Transformation[Vector[A], Constraint.Collection, NonEmptyList[A], Set[A]] =
    vector[A].validate_(uniqueItems).imap(_.to(Set))(_.toVector)

  def sortedSet[A: Order]: Transformation[Vector[A], Constraint.Collection, NonEmptyList[A], SortedSet[A]] =
    set[A].imap(SortedSet.from)(_.toSet)

  def nonEmptySet[A: Order]: Transformation[Vector[A], Constraint.Collection, NonEmptyList[A] | Long, NonEmptySet[A]] =
    sortedSet[A].ivalidate(nonEmpty)({ case (a, as) => as + a }).imap(NonEmptySet.apply)(fa => (fa.head, fa.tail))
