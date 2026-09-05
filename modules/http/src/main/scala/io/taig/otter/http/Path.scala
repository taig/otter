package io.taig.otter.http

import cats.data.Chain
import io.taig.otter as Self
import io.taig.otter.Annotation
import io.taig.otter.Reference
import io.taig.otter.Wrapper
import io.taig.otter.operation.*

/** A path that round trips `A`. */
type Path[A] = Path.Of[Segment.Node, A]

object Path:
  /** A path holding `S` and round tripping `A`.
    *
    * A path is a [[Self.Tuple]] of [[Segment]]s, which is what it is: an ordered product read out of a sequence, whose
    * positions are addressed by where they are rather than by any name. The tuple's `Empty` is `/`, and its `Zip` is
    * what makes `:*` work down the chain.
    */
  type Of[S[-w, +r] <: Segment.Node[w, r], A] = Path.Schema[S, A, A]

  /** Holding anything, which is the form an interpreter is written against. */
  type Node = [w, r] =>> Path.Schema[Segment.Node, w, r]

  /** The `S` of a node holding both an `S1` and an `S2`, which is what `:*` accumulates. */
  type Or[S1[-w, +r] <: Segment.Node[w, r], S2[-w, +r] <: Segment.Node[w, r]] = [w, r] =>> S1[w, r] | S2[w, r]

  type Reader[+A] = Path.Reader.Of[Segment.Node, A]

  object Reader:
    type Of[S[-w, +r] <: Segment.Node[w, r], +A] = Path.Schema[S, Nothing, A]

  type Writer[-A] = Path.Writer.Of[Segment.Node, A]

  object Writer:
    type Of[S[-w, +r] <: Segment.Node[w, r], -A] = Path.Schema[S, A, Any]

  /** Every segment, in the order the path names them.
    *
    * [[Self.Tuple]] already walks the product and reads through the wrappers, so this is the widening and nothing else.
    * It is here rather than written out twice because two callers need the same walk for opposite reasons: a renderer
    * asking what a path looks like with no request in hand, and a router asking whether a request is this path at all.
    */
  def segments(schema: Path.Node[?, ?]): Chain[Segment.Node[?, ?]] = schema.self.self.schemas.map(_.value)

  final case class Schema[+S[-w, +r] <: Segment.Schema[?, w, r], -W, +R](self: Annotation[Self.Tuple[S, W, R]])

  object Schema
      extends Wrapper.Tuple[Segment.Node, Path.Schema](
        [s[-w, +r] <: Segment.Node[w, r], w, r] =>
          (annotation: Annotation[Self.Tuple[s, w, r]]) => new Path.Schema(annotation),
        [s[-w, +r] <: Segment.Node[w, r], w, r] => (path: Path.Schema[s, w, r]) => path.self
      ):
    given tupleable: [S[-w, +r] <: Segment.Node[w, r]]
      => TupleableOperation[[w, r] =>> Path.Schema[S, w, r], [w, r] =>> Path.Schema[S, w, r]] =
      TupleableOperation.identity

    /** `path :* segment`. */
    given appendable: [S1[-w, +r] <: Segment.Node[w, r], S2[-w, +r] <: Segment.Node[w, r]]
        => AppendableOperation[
          [w, r] =>> Path.Schema[S1, w, r],
          [w, r] =>> Path.Schema[Path.Or[S1, S2], w, r],
          S2
        ]:
      override def lift[W, R](fa: Path.Schema[S1, W, R]): Path.Schema[Path.Or[S1, S2], W, R] = fa

      override def element[W, R](fb: => S2[W, R]): Path.Schema[Path.Or[S1, S2], W, R] =
        Path.Schema.apply[Path.Or[S1, S2], W, R](Self.Tuple.Root(Reference.later(fb)))
