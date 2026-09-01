package io.taig.otter.syntax

import cats.arrow.Profunctor
import io.taig.otter.Alt
import io.taig.otter.Append
import io.taig.otter.Convert
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
    def to[B](using w: Convert[W, B], r: Convert[R, B], P: Profunctor[F]): F[B, B] =
      P.dimap(fa)(w.from)(r.to)

    /** Maps only the read side; for a schema that cannot be written. */
    def mapTo[B](using r: Convert[R, B], P: Profunctor[F]): F[W, B] = P.rmap(fa)(r.to)

    /** Maps only the write side; for a schema that cannot be read. */
    def contramapTo[B](using w: Convert[W, B], P: Profunctor[F]): F[B, R] = P.lmap(fa)(w.from)

object OtterSyntax extends OtterSyntax
