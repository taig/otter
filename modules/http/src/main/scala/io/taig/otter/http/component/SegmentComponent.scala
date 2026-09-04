package io.taig.otter.http.component

import cats.Eq
import cats.Eval
import io.taig.otter.Reference
import io.taig.otter.http.Parameter
import io.taig.otter.http.Segment
import io.taig.otter.operation.BranchOperation
import io.taig.otter.operation.ConstantOperation
import io.taig.otter.operation.PrimitiveOperation
import io.taig.validation.Validation

/** The two ways to spell a path segment.
  *
  * `segment("users")` is the literal and `segment("id", int)` the placeholder, which is how a path reads aloud. Both
  * are `apply`, because a reader scanning `segment("users") :* segment("id", int)` cares which position is which and
  * not which constructor built it.
  */
trait SegmentComponent(using
    C: ConstantOperation[
      [w, r] =>> Segment.Static.Schema[Parameter.Primitive.Text.Node, w, r],
      Parameter.Primitive.Text.Node
    ],
    T: PrimitiveOperation.Text[Parameter.Primitive.Text.Schema]
):
  /** A literal segment, which contributes nothing to what the path holds.
    *
    * `Eq.fromUniversalEquals` rather than a summoned `Eq[String]`, since the literal is text and text is what universal
    * equality is exactly right for.
    */
  def apply(name: String): Segment.Static.Of[Parameter.Primitive.Text.Node] =
    C.lift(
      Reference.now(T.string(Validation.valid)),
      Eval.now(name),
      Eq.fromUniversalEquals[String]
    )

  /** A named segment holding whatever stands in that position. */
  def apply[S[-w, +r] <: Parameter.Value.Node[w, r], W, R](name: String, parameter: => S[W, R])(using
      B: BranchOperation[[w, r] =>> Segment.Dynamic.Schema[S, w, r], S]
  ): Segment.Dynamic.Schema[S, W, R] = B.lift(name, Reference.later(parameter))
