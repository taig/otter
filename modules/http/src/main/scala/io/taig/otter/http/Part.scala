package io.taig.otter.http

import io.taig.otter as Self
import io.taig.otter.Annotation
import io.taig.otter.Reference
import io.taig.otter.Wrapper
import io.taig.otter.operation.*

/** One part of a [[Multipart]] body, whose body requires `S`, round tripping `A`. */
type Part[S[-w, +r], A] = Part.Of[S, A]

object Part:
  /** A part holding the body `B` and round tripping `A`.
    *
    * A [[Self.Field]] over a [[Body]]: a name, a body, and the two decorations a field carries, so a part that need not
    * be sent is `optional` and one standing for a value when it is not is `optional(default)`. The body is the part's
    * own, which is where a per part `Content-Type` comes from -- there is nowhere else for it to live, and its absence
    * from a flat form alphabet is why neither earlier attempt could describe a file upload.
    */
  /** A part whose body requires `S`, which is what [[Multipart.Over]] is one of. */
  type Over[S[-w, +r]] = [w, r] =>> Part.Schema[Body.Schema[S, *, *], w, r]

  type Of[S[-w, +r], A] = Part.Over[S][A, A]

  /** Holding anything, which is the form an interpreter is written against. */
  type Node = [w, r] =>> Part.Schema[Body.Node, w, r]

  type Reader[+A] = Part.Reader.Of[Body.Payload, A]

  object Reader:
    type Of[S[-w, +r], +A] = Part.Over[S][Nothing, A]

  type Writer[-A] = Part.Writer.Of[Body.Payload, A]

  object Writer:
    type Of[S[-w, +r], -A] = Part.Over[S][A, Any]

  final case class Schema[+B[-_, +_], -W, +R](self: Annotation[Self.Field[B, W, R]])

  object Schema
      extends Wrapper.Field[Body.Node, Part.Schema](
        [b[-w, +r] <: Body.Node[w, r], w, r] =>
          (annotation: Annotation[Self.Field[b, w, r]]) => new Part.Schema(annotation),
        [b[-w, +r] <: Body.Node[w, r], w, r] => (part: Part.Schema[b, w, r]) => part.self
      ):
    given recordable: [B[-w, +r] <: Body.Node[w, r]]
      => RecordableOperation[Part.Schema[B, *, *], Multipart.Schema[B, *, *]] =
      RecordableOperation.derived

    /** `part :* part`. */
    given appendable: [B1[-w, +r] <: Body.Node[w, r], B2[-w, +r] <: Body.Node[w, r]]
        => AppendableOperation[
          Part.Schema[B1, *, *],
          Multipart.Schema[Multipart.Or[B1, B2], *, *],
          Part.Schema[B2, *, *]
        ]:
      override def lift[W, R](fa: Part.Schema[B1, W, R]): Multipart.Schema[Multipart.Or[B1, B2], W, R] =
        Multipart.Schema.apply[Multipart.Or[B1, B2], W, R](Self.Record.Root(Reference.now(fa)))

      override def element[W, R](fb: => Part.Schema[B2, W, R]): Multipart.Schema[Multipart.Or[B1, B2], W, R] =
        Multipart.Schema.apply[Multipart.Or[B1, B2], W, R](Self.Record.Root(Reference.later(fb)))
