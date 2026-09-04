package io.taig.otter.http

import io.taig.otter as Self
import io.taig.otter.Annotation
import io.taig.otter.Reference
import io.taig.otter.Wrapper
import io.taig.otter.operation.*

/** A set of body parts that round trips `A`. */
type Multipart[A] = Multipart.Of[Body.Node, A]

object Multipart:
  /** Parts holding the body `B` and round tripping `A`.
    *
    * A [[Self.Record]] of [[Part]]s, which is what a multipart body is: named members, each a body in its own right. It
    * is not a case of [[Body]] but a *payload* for one, because that is what it is in HTTP too -- a body whose content
    * happens to be a set of bodies. Saying it that way is what makes it nest for free, which multipart does, and what
    * keeps [[Body]] to the three forms bytes can actually arrive in.
    *
    * This is the `×` the body algebra needed. Alternatives were always expressible -- [[Bodies]] is a union -- but a
    * product of bodies was not, and a multipart body is nothing else.
    */
  type Of[B[-w, +r], A] = Multipart.Schema[B, A, A]

  /** Holding anything, which is the form an interpreter is written against. */
  type Node = [w, r] =>> Multipart.Schema[Body.Node, w, r]

  /** The `S` of a node holding both a `B1` and a `B2`, which is what `:*` accumulates over parts. */
  type Or[B1[-w, +r], B2[-w, +r]] = [w, r] =>> B1[w, r] | B2[w, r]

  type Reader[+A] = Multipart.Reader.Of[Body.Node, A]

  object Reader:
    type Of[B[-w, +r], +A] = Multipart.Schema[B, Nothing, A]

  type Writer[-A] = Multipart.Writer.Of[Body.Node, A]

  object Writer:
    type Of[B[-w, +r], -A] = Multipart.Schema[B, A, Any]

  final case class Schema[+B[-w, +r], -W, +R](
      self: Annotation[Self.Record[[w, r] =>> Part.Schema[B, w, r], W, R]]
  )

  object Schema
      extends Wrapper.Record[Body.Node, Multipart.Schema, Part.Schema](
        [b[-w, +r] <: Body.Node[w, r], w, r] =>
          (annotation: Annotation[Self.Record[[a, c] =>> Part.Schema[b, a, c], w, r]]) =>
            new Multipart.Schema(annotation),
        [b[-w, +r] <: Body.Node[w, r], w, r] => (multipart: Multipart.Schema[b, w, r]) => multipart.self
      ):
    given recordable: [B[-w, +r] <: Body.Node[w, r]]
      => RecordableOperation[[w, r] =>> Multipart.Schema[B, w, r], [w, r] =>> Multipart.Schema[B, w, r]] =
      RecordableOperation.identity

    /** `parts :* part`. */
    given appendable: [B1[-w, +r] <: Body.Node[w, r], B2[-w, +r] <: Body.Node[w, r]]
        => AppendableOperation[
          [w, r] =>> Multipart.Schema[B1, w, r],
          [w, r] =>> Multipart.Schema[Multipart.Or[B1, B2], w, r],
          [w, r] =>> Part.Schema[B2, w, r]
        ]:
      override def lift[W, R](fa: Multipart.Schema[B1, W, R]): Multipart.Schema[Multipart.Or[B1, B2], W, R] = fa

      override def element[W, R](fb: => Part.Schema[B2, W, R]): Multipart.Schema[Multipart.Or[B1, B2], W, R] =
        Multipart.Schema.apply[Multipart.Or[B1, B2], W, R](Self.Record.Root(Reference.later(fb)))
