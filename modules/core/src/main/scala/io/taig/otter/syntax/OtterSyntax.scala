package io.taig.otter.syntax

import cats.arrow.Profunctor
import io.taig.otter.Alt
import io.taig.otter.Append
import io.taig.otter.Convert
import io.taig.otter.Reference
import io.taig.otter.Zip
import io.taig.otter.operation.*

trait OtterSyntax:
  extension [F[- _, + _], W1, R1](fa: F[W1, R1])
    /** Appends a field to a record.
      *
      * One operator serves both `field :* field` and `record :* field`: the receiver is lifted into a record first,
      * which is the identity when it already is one.
      */
    inline def :*[G[- _, + _] <: Matchable, H[- _, + _], W2, R2](fb: => H[W2, R2])(using
        R: RecordableOperation[F, G],
        O: RecordOperation[G, H],
        P: Profunctor[G],
        Z: Zip[G]
    ): G[Append[W1, W2], Append[R1, R2]] = Append(R.toRecord(fa), O.lift(Reference.later(fb)))

    /** Appends a branch to a union, lifting the receiver into a union first. */
    def :+[G[- _, + _], H[- _, + _], W2, R2](fb: => H[W2, R2])(using
        U: UnionableOperation[F, G],
        O: UnionOperation[G, H],
        A: Alt[G]
    ): G[Either[W1, W2], Either[R1, R2]] = A.alt(U.toUnion(fa), O.lift(Reference.later(fb)))

  extension [F[- _, + _], W, R](fa: F[W, R])
    /** Maps a round tripping schema onto a nominal type. */
    def to[B](using w: Convert[W, B], r: Convert[R, B], P: Profunctor[F]): F[B, B] =
      P.dimap(fa)(w.from)(r.to)

    /** Maps only the read side; for a schema that cannot be written. */
    def mapTo[B](using r: Convert[R, B], P: Profunctor[F]): F[W, B] = P.rmap(fa)(r.to)

    /** Maps only the write side; for a schema that cannot be read. */
    def contramapTo[B](using w: Convert[W, B], P: Profunctor[F]): F[B, R] = P.lmap(fa)(w.from)

object OtterSyntax extends OtterSyntax
