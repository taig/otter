package io.taig.otter.http

import io.taig.otter as Self
import io.taig.otter.Annotation
import io.taig.otter.Reference
import io.taig.otter.Wrapper
import io.taig.otter.operation.*

/** A query string that round trips `A`. */
type Queries[A] = Queries.Of[Parameter.Node, A]

object Queries:
  /** A set of query parameters holding `S` and round tripping `A`.
    *
    * A [[Self.Record]] of [[Query]]s, which is what it is: an unordered product addressed by name, whose `Empty` is the
    * set that asks for nothing.
    */
  type Of[S[-w, +r] <: Parameter.Node[w, r], A] = Queries.Schema[S, A, A]

  /** Holding anything, which is the form an interpreter is written against. */
  type Node = [w, r] =>> Queries.Schema[Parameter.Node, w, r]

  type Reader[+A] = Queries.Reader.Of[Parameter.Node, A]

  object Reader:
    type Of[S[-w, +r] <: Parameter.Node[w, r], +A] = Queries.Schema[S, Nothing, A]

  type Writer[-A] = Queries.Writer.Of[Parameter.Node, A]

  object Writer:
    type Of[S[-w, +r] <: Parameter.Node[w, r], -A] = Queries.Schema[S, A, Any]

  final case class Schema[+S[-w, +r] <: Parameter.Schema[?, w, r], -W, +R](
      self: Annotation[Self.Record[[w, r] =>> Query.Schema[S, w, r], W, R]]
  )

  object Schema
      extends Wrapper.Record[Parameter.Node, Queries.Schema, Query.Schema](
        [s[-w, +r] <: Parameter.Node[w, r], w, r] =>
          (annotation: Annotation[Self.Record[[a, b] =>> Query.Schema[s, a, b], w, r]]) =>
            new Queries.Schema(annotation),
        [s[-w, +r] <: Parameter.Node[w, r], w, r] => (querys: Queries.Schema[s, w, r]) => querys.self
      ):
    given recordable: [S[-w, +r] <: Parameter.Node[w, r]]
      => RecordableOperation[[w, r] =>> Queries.Schema[S, w, r], [w, r] =>> Queries.Schema[S, w, r]] =
      RecordableOperation.identity

    /** `querys :* query`. The result carries both children's `S`, so the union accumulates down the chain. */
    given appendable: [S1[-w, +r] <: Parameter.Node[w, r], S2[-w, +r] <: Parameter.Node[w, r]]
        => AppendableOperation[
          [w, r] =>> Queries.Schema[S1, w, r],
          [w, r] =>> Queries.Schema[Parameter.Or[S1, S2], w, r],
          [w, r] =>> Query.Schema[S2, w, r]
        ]:
      override def lift[W, R](fa: Queries.Schema[S1, W, R]): Queries.Schema[Parameter.Or[S1, S2], W, R] = fa

      override def element[W, R](fb: => Query.Schema[S2, W, R]): Queries.Schema[Parameter.Or[S1, S2], W, R] =
        Queries.Schema.apply[Parameter.Or[S1, S2], W, R](Self.Record.Root(Reference.later(fb)))
