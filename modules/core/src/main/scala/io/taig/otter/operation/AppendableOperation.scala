package io.taig.otter.operation

import io.taig.otter.Reference

/** What `:*` needs in order to append `H` to `F`: a way to lift the receiver into the container `G` that accumulates,
  * and a way to lift an element into it.
  *
  * Records and tuples both feed this, which is why one operator serves `field :* field` and `TNil :* string` alike.
  * What keeps a format's instances apart is its own alphabet: a field is not a schema, a cell is not a row, a segment
  * is not a path. JSON has no such tier -- a tuple is a schema like any other -- so there the instance for a receiver
  * that is not yet a tuple asks `NotGiven` to stand down for the one that is.
  *
  * `F` is the container and `H` the element, rather than the left and the right of an operator, and that is what lets
  * `*:` reuse every instance registered here. `:*` searches keyed on its receiver and `*:` on its argument, which is
  * the same question asked from the other side.
  */
trait AppendableOperation[F[-_, +_], G[-_, +_], -H[-_, +_]]:
  def lift[W, R](fa: F[W, R]): G[W, R]

  def element[W, R](fb: => H[W, R]): G[W, R]

object AppendableOperation:
  inline def apply[F[-_, +_], G[-_, +_], H[-_, +_]](using
      self: AppendableOperation[F, G, H]
  ): AppendableOperation[F, G, H] = self

  /** These are deliberately not `given`s. Offering both to implicit search makes it commit to the first candidate and
    * fail instead of falling through to the second, so a format registers the one instance that fits each receiver in
    * that receiver's companion.
    */
  def record[F[-_, +_], G[-_, +_], H[-_, +_]](using
      R: RecordableOperation[F, G],
      O: RecordOperation[G, H]
  ): AppendableOperation[F, G, H] = new AppendableOperation[F, G, H]:
    override def lift[W, R](fa: F[W, R]): G[W, R] = R.toRecord(fa)
    override def element[W, R](fb: => H[W, R]): G[W, R] = O.lift(Reference.later(fb))

  def tuple[F[-_, +_], G[-_, +_], H[-_, +_]](using
      T: TupleableOperation[F, G],
      O: TupleOperation[G, H]
  ): AppendableOperation[F, G, H] = new AppendableOperation[F, G, H]:
    override def lift[W, R](fa: F[W, R]): G[W, R] = T.toTuple(fa)
    override def element[W, R](fb: => H[W, R]): G[W, R] = O.lift(Reference.later(fb))
