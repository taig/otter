package io.taig.otter.component

import io.taig.otter.Reference
import io.taig.otter.codec.Encoder
import io.taig.otter.operation.BranchOperation

/** `F` is the branch node, applied to the type of the schema the branch holds. `K` is the type a name may be spelled
  * with, which is a text primitive.
  *
  * A name is never written out and never read back: it labels the path a violation is reported at, and that is a
  * `String`. So a typed name is printed here, while the schema is being built, rather than carried into the node for an
  * interpreter to print later.
  */
trait BranchComponent[Bound[-_, +_], K[-_, +_], F[_[-w, +r] <: Bound[w, r], -_, +_]](using key: Encoder[K, String]):
  def apply[S[-w, +r] <: Bound[w, r], W, R](name: String, schema: => S[W, R])(using
      F: BranchOperation[[w, r] =>> F[S, w, r], S]
  ): F[S, W, R] = F.lift(name, Reference.later(schema))

  /** The name's schema is only ever written, so its read side is left open and a [[PrimitiveComponent.Text.printer]]
    * names a branch as readily as a round tripping schema does. It is taken by value rather than by name because,
    * unlike the schema the branch holds, it is forced immediately and can never be recursive.
    */
  def apply[S[-w, +r] <: Bound[w, r], N, W, R](name: N, tpe: K[N, Any], schema: => S[W, R])(using
      F: BranchOperation[[w, r] =>> F[S, w, r], S]
  ): F[S, W, R] = F.lift(key.encode(tpe, name), Reference.later(schema))
