package io.taig.otter.operation

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
  *
  * There are no instances here. A format registers the one that fits each receiver in that receiver's own companion,
  * where the alphabet that tells them apart is in scope.
  */
trait AppendableOperation[F[-_, +_], G[-_, +_], -H[-_, +_]] extends AppendableOperation.For[F, H]:
  override type Container[-w, +r] = G[w, r]

object AppendableOperation:
  /** Selects the container from the operands before checking the expected result type.
    *
    * The value types of `:*` and `*:` depend on shape evidence. While they are unresolved, inferring a free `G` from
    * the expected type can constrain its entire constructor instead of this application, rejecting a union of narrow
    * alphabets that would fit once applied. Searching by `F` and `H` keeps that result out of instance selection;
    * [[AppendableOperation]] still lets instances name their container directly.
    */
  trait For[F[-_, +_], -H[-_, +_]]:
    type Container[-w, +r]

    def lift[W, R](fa: F[W, R]): Container[W, R]

    def element[W, R](fb: => H[W, R]): Container[W, R]

  inline def apply[F[-_, +_], G[-_, +_], H[-_, +_]](using
      self: AppendableOperation[F, G, H]
  ): AppendableOperation[F, G, H] = self
