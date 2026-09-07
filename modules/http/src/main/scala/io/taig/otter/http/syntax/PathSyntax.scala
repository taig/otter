package io.taig.otter.http.syntax

import cats.Eq
import cats.Eval
import cats.arrow.Profunctor
import io.taig.otter.Append
import io.taig.otter.Reference
import io.taig.otter.Zip
import io.taig.otter.http.Parameter
import io.taig.otter.http.Path
import io.taig.otter.http.Segment
import io.taig.otter.operation.AppendableOperation
import io.taig.otter.operation.ConstantOperation
import io.taig.otter.operation.PrimitiveOperation
import io.taig.validation.Validation

/** `/`, which is how a path is written down everywhere outside a schema.
  *
  * It is `:*` under another name and with a narrower type, and deliberately nothing more: the very same
  * [[io.taig.otter.operation.AppendableOperation]] instances answer it, so `segment("users") / segment("id", int)` and
  * `PNil :* segment("users") :* segment("id", int)` are one schema built by one code path rather than two spellings
  * that have to be kept in step. Everything `:*` does it therefore does too -- a receiver that already is a path keeps
  * appending into itself, two segments beside each other are the path that holds them, and a literal stays out of the
  * value type because [[io.taig.otter.Append]] drops the `Unit` a [[Segment.Static]] erases to.
  *
  * What narrows it is the element and the result, not the receiver. The element is bounded to a [[Segment]] and the
  * result to a [[Path]], which is the whole of what this operator may build; the receiver is left as
  * [[io.taig.otter.syntax.OtterSyntax]] leaves it, because bounding it is what would cost precision -- a bound is an
  * upper bound, and inferring `PNil` against one widens what it holds from nothing to any segment at all, where
  * unifying against the instance keeps it exact.
  *
  * A literal may be given as a bare [[String]], which is the one place a schema is not asked for by name. There is
  * nothing left to say about a position holding a fixed piece of text, and `/ "users"` builds what `segment("users")`
  * builds, through the same two operations [[io.taig.otter.http.component.SegmentComponent]] is written against.
  *
  * Two things Scala gives it for nothing. `/` binds at the precedence of `*` and `/`, which is tighter than `:*` and
  * `++`, so a path assembled where it is used needs no parentheses around it. And it is left-associative, which is the
  * direction a URL is read in, where `*:` -- ending in a colon -- is not.
  */
trait PathSyntax:
  extension [F[-_, +_], W1, R1](fa: F[W1, R1])
    /** `segment / segment` and `path / segment`, both of which are the path holding what they name. */
    def /[G[-w, +r] <: Path.Node[w, r], H[-w, +r] <: Segment.Node[w, r], W2, R2](fb: => H[W2, R2])(using
        A: AppendableOperation[F, G, H],
        P: Profunctor[G],
        Z: Zip[G],
        W: Append.Shape[W1, W2],
        R: Append.Shape[R1, R2]
    ): G[Append[W1, W2], Append[R1, R2]] = Append(A.lift(fa), A.element(fb))

    /** The same, with a segment the request has to spell exactly: `PNil / "users" / segment("id", int)`. */
    def /[G[-w, +r] <: Path.Node[w, r]](name: String)(using
        A: AppendableOperation[F, G, PathSyntax.Literal],
        P: Profunctor[G],
        Z: Zip[G],
        C: ConstantOperation[PathSyntax.Literal, Parameter.Primitive.Text.Node],
        T: PrimitiveOperation.Text[Parameter.Primitive.Text.Schema],
        W: Append.Shape[W1, Unit],
        R: Append.Shape[R1, Unit]
    ): G[Append[W1, Unit], Append[R1, Unit]] = fa / PathSyntax.literal(name)

object PathSyntax extends PathSyntax:
  /** What a bare [[String]] in a path stands for: a [[Segment.Static]] spelled out as text. */
  type Literal = [w, r] =>> Segment.Static.Schema[Parameter.Primitive.Text.Node, w, r]

  /** What `segment(name)` builds, which is what a bare literal has to be if the two are to be one thing.
    *
    * `Eq.fromUniversalEquals` rather than a summoned `Eq[String]`, for the reason
    * [[io.taig.otter.http.component.SegmentComponent]] gives: the literal is text, and text is what universal equality
    * is exactly right for.
    */
  private def literal(name: String)(using
      C: ConstantOperation[PathSyntax.Literal, Parameter.Primitive.Text.Node],
      T: PrimitiveOperation.Text[Parameter.Primitive.Text.Schema]
  ): Segment.Static.Of[Parameter.Primitive.Text.Node] =
    C.lift(Reference.now(T.string(Validation.valid)), Eval.now(name), Eq.fromUniversalEquals[String])
