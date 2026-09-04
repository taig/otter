package io.taig.otter.syntax

import cats.arrow.Profunctor
import io.taig.otter.Alt
import io.taig.otter.Append
import io.taig.otter.Convert
import io.taig.otter.Prepend
import io.taig.otter.Zip
import io.taig.otter.operation.*

trait OtterSyntax:
  extension [F[-_, +_], W1, R1](fa: F[W1, R1])
    /** Appends a field to a record, or a schema to a tuple.
      *
      * One operator serves `field :* field`, `record :* field` and `TNil :* string`: the receiver is lifted into the
      * container that accumulates, which is the identity when it already is one.
      */
    def :*[G[-_, +_], H[-_, +_], W2, R2](fb: => H[W2, R2])(using
        A: AppendableOperation[F, G, H],
        P: Profunctor[G],
        Z: Zip[G],
        W: Append.Shape[W1, W2],
        R: Append.Shape[R1, R2]
    ): G[Append[W1, W2], Append[R1, R2]] = Append(A.lift(fa), A.element(fb))

    /** Prepends a field to a record, or a schema to a tuple: `foo *: bar *: TNil`, which is `TNil :* foo :* bar` the
      * way Scala spells a cons.
      *
      * It needs no instances of its own. [[io.taig.otter.operation.AppendableOperation]] names a container and an
      * element rather than a left and a right, and which operand supplies which is all the two operators disagree
      * about, so searching it keyed on the argument finds every instance `:*` finds keyed on the receiver.
      *
      * The left operand is strict, where `:*` suspends its right one: it is the extension parameter, and Scala
      * evaluates the left operand of a right-associative operator first. Recursion is not what that gives up -- a
      * schema referring to itself is suspended where it already is, in the child position `field` or `collection.list`
      * wraps in a [[io.taig.otter.Reference]]. A schema named further down the file and appended bare is: write that
      * one with `:*`, which suspends its element.
      */
    def *:[G[-_, +_], H[-_, +_], W2, R2](fb: H[W2, R2])(using
        A: AppendableOperation[H, G, F],
        P: Profunctor[G],
        Z: Zip[G],
        W: Prepend.Shape[W1, W2],
        R: Prepend.Shape[R1, R2]
    ): G[Prepend[W1, W2], Prepend[R1, R2]] = Prepend(A.element(fa), A.lift(fb))

    /** Appends a branch to a union, lifting the receiver into a union first. */
    def :+[G[-_, +_], H[-_, +_], W2, R2](fb: => H[W2, R2])(using
        A: AlternableOperation[F, G, H],
        L: Alt[G]
    ): G[Either[W1, W2], Either[R1, R2]] = L.alt(A.lift(fa), A.element(fb))

    /** Puts a whole record beside another, where `:*` puts a single field beside one.
      *
      * That is what a schema decorating another schema is made of: a summary with a distance written in front of it, a
      * union branch that writes a type's members beside the record's own rather than nested under a key.
      *
      * The pair stays nested, where `:*` flattens. Flattening is [[io.taig.otter.Append]]'s decision and it turns on
      * whether the left side already is a tuple, so routing this through it would leave the shape a call site sees to
      * depend on what happens to be standing on the left.
      */
    def zip[W2, R2](fb: F[W2, R2])(using Z: Zip[F]): F[(W1, W2), (R1, R2)] = Z.zip(fa, fb)

    /** [[zip]] written as an operator, so that a schema assembled from several reads as one expression rather than as a
      * chain of calls.
      *
      * `++` rather than a third appending operator, because concatenating what the container holds is the part all
      * three have in common: `record ++ record` writes both sets of fields into one object, as `field :* field` does.
      * What the two operands are to each other is the part that differs, and it is the Scala value that says so --
      * nested here, flat under `:*`, for the reason [[zip]] gives.
      *
      * Precedence puts it between the two: tighter than `:*`, so `record ++ record :* field` zips before it appends,
      * and looser than `*:`. What it appends to is a pair, which [[io.taig.otter.Append]] then flattens like any other
      * tuple standing on the left, so that reads `(A, B, C)` rather than `((A, B), C)`.
      *
      * Both operands have to be the same container, which `toRecord` and `toTuple` are for.
      */
    def ++[W2, R2](fb: F[W2, R2])(using Z: Zip[F]): F[(W1, W2), (R1, R2)] = Z.zip(fa, fb)

  extension [F[-_, +_], W, R](fa: F[W, R])
    /** Maps the read side. What the schema writes is forgotten rather than broken, so the result is a reader: mapping
      * one side of a round trip leaves something that no longer round trips, and the type has to say so.
      *
      * Forgetting is only the honest answer when `f` loses what the write side would need. A normalisation -- a trim, a
      * case fold -- does not: the wire text is still writable, and
      * [[io.taig.otter.component.PrimitiveComponent.Text.normalized]] keeps it. Reach for that first, and for
      * [[io.taig.otter.component.PrimitiveComponent.Text.codec]] whenever a print function exists at all.
      */
    def map[B](f: R => B)(using P: Profunctor[F]): F[Nothing, B] = P.rmap(fa)(f)

    /** Maps the write side, leaving a writer for the same reason. */
    def contramap[B](f: B => W)(using P: Profunctor[F]): F[B, Any] = P.lmap(fa)(f)

    /** Maps a round tripping schema onto a nominal type. */
    def to[B](using w: Convert[W, B], r: Convert.Reader[R, B], P: Profunctor[F]): F[B, B] =
      P.dimap(fa)(w.from)(r.to)

    /** Maps only the read side; for a schema that cannot be written. */
    def mapTo[B](using r: Convert.Reader[R, B], P: Profunctor[F]): F[W, B] = P.rmap(fa)(r.to)

    /** Maps only the write side; for a schema that cannot be read. */
    def contramapTo[B](using w: Convert[W, B], P: Profunctor[F]): F[B, R] = P.lmap(fa)(w.from)

object OtterSyntax extends OtterSyntax
