package io.taig.otter.http

import io.taig.otter as Self
import io.taig.otter.Annotation
import io.taig.otter.Reference
import io.taig.otter.Wrapper
import io.taig.otter.operation.*

/** A header that round trips `A`. */
type Header[A] = Header.Of[Parameter.Node, A]

object Header:
  /** A header holding `S` and round tripping `A`.
    *
    * A header is a [[Self.Field]]: a name, a schema, and the two decorations a field carries. `optional` is a header
    * that may not be given at all, and `optional(default)` is one that stands for a value when it is not -- the same
    * read and write asymmetry a JSON field has, since a default is always written out and never required when read.
    */
  type Of[S[-w, +r] <: Parameter.Node[w, r], A] = Header.Schema[S, A, A]

  /** Holding anything, which is the form an interpreter is written against. */
  type Node = [w, r] =>> Header.Schema[Parameter.Node, w, r]

  type Reader[+A] = Header.Reader.Of[Parameter.Node, A]

  object Reader:
    type Of[S[-w, +r] <: Parameter.Node[w, r], +A] = Header.Schema[S, Nothing, A]

  type Writer[-A] = Header.Writer.Of[Parameter.Node, A]

  object Writer:
    type Of[S[-w, +r] <: Parameter.Node[w, r], -A] = Header.Schema[S, A, Any]

  final case class Schema[+S[-w, +r] <: Parameter.Schema[?, w, r], -W, +R](self: Annotation[Self.Field[S, W, R]])

  object Schema
      extends Wrapper.Field[Parameter.Node, Header.Schema](
        [s[-w, +r] <: Parameter.Node[w, r], w, r] =>
          (annotation: Annotation[Self.Field[s, w, r]]) => new Header.Schema(annotation),
        [s[-w, +r] <: Parameter.Node[w, r], w, r] => (header: Header.Schema[s, w, r]) => header.self
      ):
    given recordable: [S[-w, +r] <: Parameter.Node[w, r]]
      => RecordableOperation[[w, r] =>> Header.Schema[S, w, r], [w, r] =>> Headers.Schema[S, w, r]] =
      RecordableOperation.derived

    /** `header :* header`. */
    given appendable: [S1[-w, +r] <: Parameter.Node[w, r], S2[-w, +r] <: Parameter.Node[w, r]]
        => AppendableOperation[
          [w, r] =>> Header.Schema[S1, w, r],
          [w, r] =>> Headers.Schema[Parameter.Or[S1, S2], w, r],
          [w, r] =>> Header.Schema[S2, w, r]
        ]:
      override def lift[W, R](fa: Header.Schema[S1, W, R]): Headers.Schema[Parameter.Or[S1, S2], W, R] =
        Headers.Schema.apply[Parameter.Or[S1, S2], W, R](Self.Record.Root(Reference.now(fa)))

      override def element[W, R](fb: => Header.Schema[S2, W, R]): Headers.Schema[Parameter.Or[S1, S2], W, R] =
        Headers.Schema.apply[Parameter.Or[S1, S2], W, R](Self.Record.Root(Reference.later(fb)))
