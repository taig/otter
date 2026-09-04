package io.taig.otter.codec

import cats.Monoid
import io.bullet.borer.Writer

/** A write, waiting for the output to write itself into.
  *
  * `Encoder` returns a value and borer writes an effect, so what this module's encoder produces is the write rather
  * than the document. Combining two of them is what building a document out of two values would otherwise have been,
  * and the combination is performed exactly once, by the output, with nothing standing between the schema and the
  * bytes. That is the whole reason the module exists: a document model is 64% to 95% of what a write costs, measured,
  * and this is what there is instead of one.
  *
  * Opaque, with its own `Monoid`, on purpose. `Writer => Writer` would otherwise be eligible for cats' pointwise
  * instance for functions, and a pointwise combine would write the members of a record over the top of each other. The
  * combine here is left to right, which is what makes a record keep its declaration order.
  */
opaque type BorerWrite = Writer => Writer

object BorerWrite:
  /** Writes nothing, which is what an empty record and an omitted field contribute. */
  val Empty: BorerWrite = identity

  def apply(write: Writer => Writer): BorerWrite = write

  extension (self: BorerWrite) def write(writer: Writer): Writer = self(writer)

  given Monoid[BorerWrite]:
    override val empty: BorerWrite = BorerWrite.Empty

    override def combine(x: BorerWrite, y: BorerWrite): BorerWrite = writer => y(x(writer))
