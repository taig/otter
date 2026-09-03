package io.taig.otter.http

import io.taig.otter as Self
import io.taig.otter.Annotation
import io.taig.otter.Reference
import io.taig.otter.Wrapper
import io.taig.otter.operation.*

/** Every answer an endpoint may give, round tripping `A`. */
type Results[A] = Results.Of[Body.Payload, A]

object Results:
  /** Answers holding the payload `S` and round tripping `A`.
    *
    * A [[Self.Union]] of [[Result]]s, one per status [[Code]]. The same node [[Bodies]] uses, one tier up: writing
    * picks the answer that matches, and reading tries them until one fits.
    */
  type Of[S[-w, +r], A] = Results.Schema[S, A, A]

  /** Holding anything, which is the form an interpreter is written against. */
  type Node = [w, r] =>> Results.Schema[Body.Payload, w, r]

  type Reader[+A] = Results.Reader.Of[Body.Payload, A]

  object Reader:
    type Of[S[-w, +r], +A] = Results.Schema[S, Nothing, A]

  type Writer[-A] = Results.Writer.Of[Body.Payload, A]

  object Writer:
    type Of[S[-w, +r], -A] = Results.Schema[S, A, Any]

  final case class Schema[+S[-w, +r], -W, +R](
      self: Annotation[Self.Union[[w, r] =>> Result.Schema[S, w, r], W, R]]
  )

  object Schema
      extends Wrapper.Union[Body.Payload, Results.Schema, Result.Schema](
        [s[-w, +r], w, r] =>
          (annotation: Annotation[Self.Union[[a, b] =>> Result.Schema[s, a, b], w, r]]) =>
            new Results.Schema(annotation),
        [s[-w, +r], w, r] => (results: Results.Schema[s, w, r]) => results.self
      ):
    given unionable: [S[-w, +r]]
      => UnionableOperation[[w, r] =>> Results.Schema[S, w, r], [w, r] =>> Results.Schema[S, w, r]] =
      UnionableOperation.identity

    /** `results :+ result`. */
    given alternable: [S1[-w, +r], S2[-w, +r]]
        => AlternableOperation[
          [w, r] =>> Results.Schema[S1, w, r],
          [w, r] =>> Results.Schema[Body.Or[S1, S2], w, r],
          [w, r] =>> Result.Schema[S2, w, r]
        ]:
      override def lift[W, R](fa: Results.Schema[S1, W, R]): Results.Schema[Body.Or[S1, S2], W, R] = fa

      override def element[W, R](fb: => Result.Schema[S2, W, R]): Results.Schema[Body.Or[S1, S2], W, R] =
        Results.Schema.apply[Body.Or[S1, S2], W, R](Self.Union.Root(Reference.later(fb)))
