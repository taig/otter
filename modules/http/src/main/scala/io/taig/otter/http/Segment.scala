package io.taig.otter.http

import cats.Contravariant
import cats.Functor
import cats.Invariant
import cats.arrow.Profunctor
import io.taig.otter as Self
import io.taig.otter.Annotation
import io.taig.otter.Direction
import io.taig.otter.Reference
import io.taig.otter.Wrapper
import io.taig.otter.operation.*

/** One piece of a [[Path]], round tripping `A`. */
type Segment[A] = Segment.Of[Parameter.Value.Node, A]

object Segment:
  /** The two things a path segment can be.
    *
    * A [[Segment.Static]] is a literal the request has to spell exactly; a [[Segment.Dynamic]] is a name and a value
    * read out of whatever stands in that position. Nothing else: a segment is bounded to a [[Parameter.Value]] rather
    * than a [[Parameter]], so a repeated segment cannot be written down, and it holds no `Optional`, because a segment
    * that may not be there is a different path and belongs in a [[Bodies]]-like alternation of its own.
    */
  sealed abstract class Schema[+S[-w, +r] <: Parameter.Value.Schema[?, w, r], -W, +R]

  /** A segment holding `S` and round tripping `A`. */
  type Of[S[-w, +r] <: Parameter.Value.Node[w, r], A] = Segment.Schema[S, A, A]

  /** Holding anything, which is the form an interpreter is written against. */
  type Node = [w, r] =>> Segment.Schema[Parameter.Value.Node, w, r]

  /** The `S` of a node holding both an `S1` and an `S2`, which is what `:*` accumulates down a path. */
  type Or[S1[-w, +r] <: Parameter.Value.Node[w, r], S2[-w, +r] <: Parameter.Value.Node[w, r]] =
    [w, r] =>> S1[w, r] | S2[w, r]

  type Reader[+A] = Segment.Reader.Of[Parameter.Value.Node, A]

  object Reader:
    type Of[S[-w, +r] <: Parameter.Value.Node[w, r], +A] = Segment.Schema[S, Nothing, A]

  type Writer[-A] = Segment.Writer.Of[Parameter.Value.Node, A]

  object Writer:
    type Of[S[-w, +r] <: Parameter.Value.Node[w, r], -A] = Segment.Schema[S, A, Any]

  type Static = Segment.Static.Schema[Parameter.Primitive.Node, Unit, Unit]

  /** A literal segment.
    *
    * [[Self.Constant]] is what a literal is, already: it writes the value it carries, requires exactly that value when
    * read, and round trips `Unit` because there is nothing left to tell the caller. Erasing to `Unit` is what keeps a
    * static segment out of the path's value type, since [[io.taig.otter.Append]] drops a `Unit` operand.
    */
  object Static:
    /** A literal holding `S`. Its `W` and `R` are `Unit`, whatever `S` spells the literal out as. */
    type Of[S[-w, +r] <: Parameter.Primitive.Node[w, r]] = Segment.Static.Schema[S, Unit, Unit]

    /** Holding anything, which is the form an interpreter is written against. */
    type Node = [w, r] =>> Segment.Static.Schema[Parameter.Primitive.Node, w, r]

    final case class Schema[+S[-w, +r] <: Parameter.Primitive.Schema[?, w, r], -W, +R](
        self: Annotation[Self.Constant[S, W, R]]
    ) extends Segment.Schema[S, W, R]

    object Schema
        extends Wrapper.Constant[Parameter.Primitive.Node, Segment.Static.Schema](
          [s[-w, +r] <: Parameter.Primitive.Node[w, r], w, r] =>
            (annotation: Annotation[Self.Constant[s, w, r]]) => new Segment.Static.Schema(annotation),
          [s[-w, +r] <: Parameter.Primitive.Node[w, r], w, r] =>
            (segment: Segment.Static.Schema[s, w, r]) => segment.self
        )

  type Dynamic[A] = Segment.Dynamic.Of[Parameter.Value.Node, A]

  /** A named segment holding a value.
    *
    * [[Self.Branch]] rather than [[Self.Field]], which is the other core node that pairs a name with a schema: a field
    * additionally offers `optional` and a default, and neither means anything in a path, where the position is either
    * occupied or the path is a different path. The name is not on the wire -- it labels the path a violation is
    * reported at, and it is what OpenAPI spells as `{name}`.
    */
  object Dynamic:
    /** A named segment holding `S` and round tripping `A`. */
    type Of[S[-w, +r] <: Parameter.Value.Node[w, r], A] = Segment.Dynamic.Schema[S, A, A]

    /** Holding anything, which is the form an interpreter is written against. */
    type Node = [w, r] =>> Segment.Dynamic.Schema[Parameter.Value.Node, w, r]

    type Reader[+A] = Segment.Dynamic.Reader.Of[Parameter.Value.Node, A]

    object Reader:
      type Of[S[-w, +r] <: Parameter.Value.Node[w, r], +A] = Segment.Dynamic.Schema[S, Nothing, A]

    type Writer[-A] = Segment.Dynamic.Writer.Of[Parameter.Value.Node, A]

    object Writer:
      type Of[S[-w, +r] <: Parameter.Value.Node[w, r], -A] = Segment.Dynamic.Schema[S, A, Any]

    final case class Schema[+S[-w, +r] <: Parameter.Value.Schema[?, w, r], -W, +R](
        self: Annotation[Self.Branch[S, W, R]]
    ) extends Segment.Schema[S, W, R]

    object Schema
        extends Wrapper.Branch[Parameter.Value.Node, Segment.Dynamic.Schema](
          [s[-w, +r] <: Parameter.Value.Node[w, r], w, r] =>
            (annotation: Annotation[Self.Branch[s, w, r]]) => new Segment.Dynamic.Schema(annotation),
          [s[-w, +r] <: Parameter.Value.Node[w, r], w, r] => (segment: Segment.Dynamic.Schema[s, w, r]) => segment.self
        )

  given profunctor: [S[-w, +r] <: Parameter.Value.Node[w, r]] => Profunctor[[w, r] =>> Segment.Schema[S, w, r]]:
    override def dimap[W0, R0, W, R](self: Segment.Schema[S, W0, R0])(f: W => W0)(g: R0 => R): Segment.Schema[S, W, R] =
      self match
        case self @ Segment.Static.Schema(_)  => Segment.Static.Schema.profunctor.dimap(self)(f)(g)
        case self @ Segment.Dynamic.Schema(_) => Segment.Dynamic.Schema.profunctor.dimap(self)(f)(g)

  given functor: [S[-w, +r] <: Parameter.Value.Node[w, r]] => Functor[[a] =>> Segment.Schema[S, Nothing, a]] =
    Direction.functor[[w, r] =>> Segment.Schema[S, w, r]]

  given contravariant: [S[-w, +r] <: Parameter.Value.Node[w, r]] => Contravariant[[a] =>> Segment.Schema[S, a, Any]] =
    Direction.contravariant[[w, r] =>> Segment.Schema[S, w, r]]

  given invariant: [S[-w, +r] <: Parameter.Value.Node[w, r]] => Invariant[[a] =>> Segment.Schema[S, a, a]] =
    Direction.invariant[[w, r] =>> Segment.Schema[S, w, r]]

  /** `segment :* segment`, and `segment *: segment`: two segments beside each other are the path that holds them, which
    * is what [[io.taig.otter.http.component.HttpComponent.PNil]] would otherwise have to be named for.
    *
    * No guard, unlike [[io.taig.otter.Json.appendable]]: a path is not a segment, so a receiver that already is one
    * falls outside this instance's bound and keeps appending into itself through [[Path.Schema.appendable]].
    */
  given appendable: [S1[-w, +r] <: Segment.Node[w, r], S2[-w, +r] <: Segment.Node[w, r]]
      => AppendableOperation[S1, [w, r] =>> Path.Schema[Path.Or[S1, S2], w, r], S2]:
    override def lift[W, R](fa: S1[W, R]): Path.Schema[Path.Or[S1, S2], W, R] =
      Path.Schema.apply[Path.Or[S1, S2], W, R](Self.Tuple.Root(Reference.now(fa)))

    override def element[W, R](fb: => S2[W, R]): Path.Schema[Path.Or[S1, S2], W, R] =
      Path.Schema.apply[Path.Or[S1, S2], W, R](Self.Tuple.Root(Reference.later(fb)))
