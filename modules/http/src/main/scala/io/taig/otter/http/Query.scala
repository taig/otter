package io.taig.otter.http

import io.taig.otter as Self
import io.taig.otter.Annotation
import io.taig.otter.Reference
import io.taig.otter.Wrapper
import io.taig.otter.operation.*

/** A query parameter that round trips `A`. */
type Query[A] = Query.Of[Parameter.Node, A]

object Query:
  /** A query parameter holding `S` and round tripping `A`.
    *
    * A query parameter is a [[Self.Field]]: a name, a schema, and the two decorations a field carries. `optional` is a
    * query parameter that may not be given at all, and `optional(default)` is one that stands for a value when it is
    * not -- the same read and write asymmetry a JSON field has, since a default is always written out and never
    * required when read.
    */
  type Of[S[-w, +r] <: Parameter.Node[w, r], A] = Query.Schema[S, A, A]

  /** Holding anything, which is the form an interpreter is written against. */
  type Node = [w, r] =>> Query.Schema[Parameter.Node, w, r]

  type Reader[+A] = Query.Reader.Of[Parameter.Node, A]

  object Reader:
    type Of[S[-w, +r] <: Parameter.Node[w, r], +A] = Query.Schema[S, Nothing, A]

  type Writer[-A] = Query.Writer.Of[Parameter.Node, A]

  object Writer:
    type Of[S[-w, +r] <: Parameter.Node[w, r], -A] = Query.Schema[S, A, Any]

  final case class Schema[+S[-w, +r] <: Parameter.Schema[?, w, r], -W, +R](self: Annotation[Self.Field[S, W, R]])

  object Schema
      extends Wrapper.Field[Parameter.Node, Query.Schema](
        [s[-w, +r] <: Parameter.Node[w, r], w, r] =>
          (annotation: Annotation[Self.Field[s, w, r]]) => new Query.Schema(annotation),
        [s[-w, +r] <: Parameter.Node[w, r], w, r] => (query: Query.Schema[s, w, r]) => query.self
      ):
    given recordable: [S[-w, +r] <: Parameter.Node[w, r]]
      => RecordableOperation[[w, r] =>> Query.Schema[S, w, r], [w, r] =>> Queries.Schema[S, w, r]] =
      RecordableOperation.derived

    /** `query :* query`. */
    given appendable: [S1[-w, +r] <: Parameter.Node[w, r], S2[-w, +r] <: Parameter.Node[w, r]]
        => AppendableOperation[
          [w, r] =>> Query.Schema[S1, w, r],
          [w, r] =>> Queries.Schema[Parameter.Or[S1, S2], w, r],
          [w, r] =>> Query.Schema[S2, w, r]
        ]:
      override def lift[W, R](fa: Query.Schema[S1, W, R]): Queries.Schema[Parameter.Or[S1, S2], W, R] =
        Queries.Schema.apply[Parameter.Or[S1, S2], W, R](Self.Record.Root(Reference.now(fa)))

      override def element[W, R](fb: => Query.Schema[S2, W, R]): Queries.Schema[Parameter.Or[S1, S2], W, R] =
        Queries.Schema.apply[Parameter.Or[S1, S2], W, R](Self.Record.Root(Reference.later(fb)))
