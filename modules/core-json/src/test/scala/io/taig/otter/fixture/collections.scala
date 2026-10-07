package io.taig.otter.fixture

import cats.Order
import cats.data.NonEmptyChain
import cats.data.NonEmptyList
import cats.data.NonEmptySet
import cats.data.NonEmptyVector
import cats.syntax.all.*
import io.taig.otter.Json
import io.taig.otter.component.JsonComponent.*
import io.taig.validation.std

import scala.collection.immutable.SortedSet

object collections:
  val linked = collection.nonEmptyList(int)
  val indexed = collection.nonEmptyVector(int)
  val chained = collection.nonEmptyChain(int)
  val sorted = collection.sortedSet(int)
  val nonEmptySorted = collection.nonEmptySet(int)

  val nonEmpty: List[Json.Reader[Any]] = List(linked, indexed, chained, nonEmptySorted)
  val bounded: List[Json.Reader[Any]] = List(
    collection.nonEmptyList(int, std.collection.maximum[NonEmptyList[Int]](2)),
    collection.nonEmptyVector(int, std.collection.maximum[NonEmptyVector[Int]](2)),
    collection.nonEmptyChain(int, std.collection.maximum[NonEmptyChain[Int]](2)),
    collection.nonEmptySet(int, std.collection.maximum[NonEmptySet[Int]](2)),
    collection.sortedSet(int, std.collection.maximum[SortedSet[Int]](2))
  )

  val byLength =
    given Order[String] = Order.by(_.length)
    collection.sortedSet(string)

  val nonEmptyByLength =
    given Order[String] = Order.by(_.length)
    collection.nonEmptySet(string)

  val reversed =
    given Order[Int] = Order.fromOrdering(using Ordering.Int.reverse)
    collection.sortedSet(int)

  val nonEmptyReversed =
    given Order[Int] = Order.fromOrdering(using Ordering.Int.reverse)
    collection.nonEmptySet(int)

  val asymmetric = collection.sortedSet(string.dimap[Int, Long](_.toString)(_.toLong))

  final case class Tree(children: NonEmptyList[Option[Tree]])
  lazy val tree: Json[Tree] = field("children", collection.nonEmptyList(tree.nullable)).toRecord.to[Tree]
