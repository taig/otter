package io.taig.otter.http

import io.taig.otter as Self
import io.taig.otter.Annotation
import io.taig.otter.Reference
import io.taig.otter.Wrapper
import io.taig.otter.operation.*

/** One part of a [[Multipart]] body, round tripping `A`. */
type Part[A] = Part.Of[Body.Node, A]

object Part:
  /** A part holding the body `B` and round tripping `A`.
    *
    * A [[Self.Field]] over a [[Body]]: a name, a body, and the two decorations a field carries, so a part that need not
    * be sent is `optional` and one standing for a value when it is not is `optional(default)`. The body is the part's
    * own, which is where a per part `Content-Type` comes from -- there is nowhere else for it to live, and its absence
    * from a flat form alphabet is why neither earlier attempt could describe a file upload.
    */
  type Of[B[-w, +r], A] = Part.Schema[B, A, A]

  /** Holding anything, which is the form an interpreter is written against. */
  type Node = [w, r] =>> Part.Schema[Body.Node, w, r]

  type Reader[+A] = Part.Reader.Of[Body.Node, A]

  object Reader:
    type Of[B[-w, +r], +A] = Part.Schema[B, Nothing, A]

  type Writer[-A] = Part.Writer.Of[Body.Node, A]

  object Writer:
    type Of[B[-w, +r], -A] = Part.Schema[B, A, Any]

  final case class Schema[+B[-w, +r], -W, +R](self: Annotation[Self.Field[B, W, R]])

  object Schema
      extends Wrapper.Field[Body.Node, Part.Schema](
        [b[-w, +r] <: Body.Node[w, r], w, r] =>
          (annotation: Annotation[Self.Field[b, w, r]]) => new Part.Schema(annotation),
        [b[-w, +r] <: Body.Node[w, r], w, r] => (part: Part.Schema[b, w, r]) => part.self
      ):
    given recordable: [B[-w, +r] <: Body.Node[w, r]]
      => RecordableOperation[[w, r] =>> Part.Schema[B, w, r], [w, r] =>> Multipart.Schema[B, w, r]] =
      RecordableOperation.derived

    /** `part :* part`. */
    given appendable: [B1[-w, +r] <: Body.Node[w, r], B2[-w, +r] <: Body.Node[w, r]]
        => AppendableOperation[
          [w, r] =>> Part.Schema[B1, w, r],
          [w, r] =>> Multipart.Schema[Multipart.Or[B1, B2], w, r],
          [w, r] =>> Part.Schema[B2, w, r]
        ]:
      override def lift[W, R](fa: Part.Schema[B1, W, R]): Multipart.Schema[Multipart.Or[B1, B2], W, R] =
        Multipart.Schema.apply[Multipart.Or[B1, B2], W, R](Self.Record.Root(Reference.now(fa)))

      override def element[W, R](fb: => Part.Schema[B2, W, R]): Multipart.Schema[Multipart.Or[B1, B2], W, R] =
        Multipart.Schema.apply[Multipart.Or[B1, B2], W, R](Self.Record.Root(Reference.later(fb)))
