package io.taig.otter.codec

import cats.data.Chain
import cats.syntax.all.*

import scala.collection.immutable.BitSet

/** The named values a record has left to read, indexed by name.
  *
  * A record reads its members one at a time, by name, and has to say afterwards which of the pairs nothing claimed.
  * Threading the pairs themselves makes every lookup a scan, so a record of `m` members over `n` pairs walks `n` pairs
  * `m` times over and rebuilds the collection at each step. The index is built once instead, and a member costs a hash.
  *
  * A name given more than once keeps every value it was given, in the order they arrived: [[take]] answers with the
  * first occurrence nothing has claimed, which is what scanning for it would have found, and the rest stay for whoever
  * asks next.
  */
final class Fields[+T] private (
    entries: Vector[(String, T)],
    positions: Map[String, List[Int]],
    claimed: BitSet
):
  /** What the name holds, and the fields left once it is taken.
    *
    * A name nothing answers to leaves the fields as they are, so a lookup that misses allocates nothing.
    */
  def take(name: String): (Fields[T], Option[T]) =
    positions.getOrElse(name, Nil).find(!claimed.contains(_)) match
      case Some(position) => (new Fields(entries, positions, claimed + position), entries(position)._2.some)
      case None           => (this, none)

  /** The pairs no name claimed, in the order they arrived.
    *
    * This is the only way the entries leave here, and what a reader that rejects a member its schema does not mention
    * asks for: [[Decoder.Remaining.verify]] is written against it.
    */
  def remainders: Chain[(String, T)] =
    if claimed.isEmpty then Chain.fromSeq(entries)
    else Chain.fromSeq(entries.zipWithIndex.collect { case (entry, position) if !claimed.contains(position) => entry })

object Fields:
  def apply[T](values: (String, T)*): Fields[T] = Fields.from(values)

  def empty[T]: Fields[T] = new Fields(Vector.empty, Map.empty, BitSet.empty)

  def from[T](values: IterableOnce[(String, T)]): Fields[T] =
    val entries = Vector.from(values)

    new Fields(entries, Fields.index(entries), BitSet.empty)

  /** Where each name sits, in the order its occurrences arrived, which is the order [[Fields.take]] hands them out in.
    */
  private def index[T](entries: Vector[(String, T)]): Map[String, List[Int]] =
    entries.indices.groupBy(entries(_)._1).map((name, positions) => name -> positions.toList)
