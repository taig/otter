package io.taig.otter.codec

import cats.data.Chain

import scala.annotation.tailrec
import scala.collection.immutable.BitSet

/** The named values a record has left to read.
  *
  * A record reads its members one at a time, by name, and has to say afterwards which of the pairs nothing claimed.
  * Threading the pairs themselves would rebuild the collection at every step, so they are held once and a position is
  * marked claimed instead. [[take]] answers with the first occurrence nothing has claimed, which is what scanning for
  * it would have found, and the rest stay where they are for whoever asks next.
  *
  * The marks are a bitmask rather than an index over the names, and nothing at all is built before a name is asked for.
  * An index has to be built out of every name before the first lookup can use it, and that cost is paid by every record
  * whether it helps or not -- it did not help: the index this replaced measured no faster than the scan *it* replaced,
  * while allocating a builder per name to exist.
  *
  * What makes the scan cheap is that positions are handed out from the front, so the lowest position nothing has
  * claimed is the lowest zero of the mask and a lookup starts there. A record whose members arrive in the order its
  * schema names them -- which is what a document written from that same schema does -- finds each of them at the first
  * position it looks at. One whose members arrive in another order walks forward, which is the walk threading the pairs
  * would have done anyway, and allocates nothing while it does it.
  *
  * The mask is an `Int` rather than a `Long` because a `Long` is a heap object on Scala.js where an `Int` is a machine
  * word on both platforms. Positions past the thirty-second are marked in a [[BitSet]] instead, which no record reaches
  * and only an enormous query string or header set does.
  */
final class Fields[+T] private (entries: Vector[(String, T)], claimed: Int, overflow: BitSet):
  private def isClaimed(position: Int): Boolean =
    if position < 32 then (claimed & (1 << position)) != 0 else overflow.contains(position)

  private def claim(position: Int): Fields[T] =
    if position < 32 then new Fields(entries, claimed | (1 << position), overflow)
    else new Fields(entries, claimed, overflow + position)

  /** What the name holds, and the fields left once it is taken.
    *
    * A name nothing answers to leaves the fields as they are, so a lookup that misses allocates nothing.
    */
  def take(name: String): (Fields[T], Option[T]) =
    // The lowest zero of the mask, which is the first position that could still answer. All thirty-two low bits
    // claimed reads as thirty-two, which is where the overflow range begins, so nothing special is needed for it.
    take(name, Integer.numberOfTrailingZeros(~claimed))

  @tailrec
  private def take(name: String, position: Int): (Fields[T], Option[T]) =
    if position >= entries.length then (this, None)
    else
      val entry = entries(position)

      if entry._1 == name && !isClaimed(position) then (claim(position), Some(entry._2))
      else take(name, position + 1)

  /** The pairs no name claimed, in the order they arrived.
    *
    * This is the only way the entries leave here, and what a reader that rejects a member its schema does not mention
    * would ask for. No format does: every one of them documents an unnamed member as something to leave where it is.
    */
  def remainders: Chain[(String, T)] =
    if claimed == 0 && overflow.isEmpty then Chain.fromSeq(entries)
    else Chain.fromSeq(entries.zipWithIndex.collect { case (entry, position) if !isClaimed(position) => entry })

object Fields:
  /** Holds nothing, so one of them serves every `T`. */
  private val Empty: Fields[Nothing] = new Fields(Vector.empty, claimed = 0, overflow = BitSet.empty)

  def apply[T](values: (String, T)*): Fields[T] = Fields.from(values)

  def empty[T]: Fields[T] = Fields.Empty

  def from[T](values: IterableOnce[(String, T)]): Fields[T] =
    val entries = Vector.from(values)

    if entries.isEmpty then Fields.Empty else new Fields(entries, claimed = 0, overflow = BitSet.empty)
