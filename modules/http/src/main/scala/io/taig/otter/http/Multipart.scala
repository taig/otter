package io.taig.otter.http

import cats.data.Chain
import io.taig.otter as Self
import io.taig.otter.Annotation
import io.taig.otter.Metadata
import io.taig.otter.Reference
import io.taig.otter.Wrapper
import io.taig.otter.operation.*

/** A set of body parts whose bodies require `S`, round tripping `A`. */
type Multipart[S[-w, +r], A] = Multipart.Of[S, A]

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
  /** Parts whose bodies require `S`, which is both what a definition ascribes and what an interpreter covers.
    *
    * The parameter [[Multipart.Schema]] itself takes is the *body* each part holds, and a body keeps its own
    * requirement, so `:*` was already accumulating what the parts need -- `Multipart.Or` unions the body functors, and
    * the requirement rides along inside them. What was missing was a name for the accumulated total that a definition
    * could be ascribed at. `Multipart[A]` used to widen every part to [[Body.Node]], whose requirement is
    * [[Body.Payload]] and therefore `Any`, and an upload written that way could be served by nothing at all.
    */
  type Over[S[-w, +r]] = [w, r] =>> Multipart.Schema[Body.Schema[S, *, *], w, r]

  /** What a multipart body over `S` asks of an interpreter: the multipart structure, and every part's own payload.
    *
    * Nested rather than laid beside the parts' requirement, because
    * [[io.taig.otter.http.codec.Http4sPayload.Of.orElse]] unions at the top:
    * `Body.Or[Body.Whole[Multipart.Requirement], S]` would let a CSV part be answered for by a CSV entry the multipart
    * interpreter had never been handed. Saying it this way, an interpreter for multipart bodies names in its own type
    * the registry it will read the parts with, so supplying one is necessary and not sufficient.
    */
  type Requirement[S[-w, +r]] = Body.Whole[Multipart.Over[S]]

  type Of[S[-w, +r], A] = Multipart.Over[S][A, A]

  /** Holding anything, which is the form an interpreter is written against. */
  type Node = [w, r] =>> Multipart.Schema[Body.Node, w, r]

  /** The `B` of a node holding both a `B1` and a `B2`, which is what `:*` accumulates over parts.
    *
    * Over body functors, and what that carries is the union of the requirements those bodies hold.
    */
  type Or[B1[-w, +r], B2[-w, +r]] = [w, r] =>> B1[w, r] | B2[w, r]

  type Reader[+A] = Multipart.Reader.Of[Body.Payload, A]

  object Reader:
    type Of[S[-w, +r], +A] = Multipart.Over[S][Nothing, A]

  type Writer[-A] = Multipart.Writer.Of[Body.Payload, A]

  object Writer:
    type Of[S[-w, +r], -A] = Multipart.Over[S][A, Any]

  /** Every part, with the metadata it carries of its own.
    *
    * The metadata comes back beside the field because it is not the body's: a filename is said about the part, and a
    * caller reading the body's metadata would find nothing there.
    */
  def parts(schema: Multipart.Node[?, ?]): Chain[(Self.Field[Body.Node, ?, ?], Metadata)] =
    Multipart.walk(schema.self.self)

  private def walk(schema: Self.Record[Part.Node, ?, ?]): Chain[(Self.Field[Body.Node, ?, ?], Metadata)] =
    schema match
      case Self.Record.Empty                => Chain.empty
      case Self.Record.Modify(self, _, _)   => Multipart.walk(self)
      case Self.Record.Product(left, right) => Multipart.walk(left) ++ Multipart.walk(right)
      case Self.Record.Root(field)          => Chain.one((field.value.self.self, field.value.self.metadata))

  final case class Schema[+B[-_, +_], -W, +R](
      self: Annotation[Self.Record[Part.Schema[B, *, *], W, R]]
  )

  object Schema
      extends Wrapper.Record[Body.Node, Multipart.Schema, Part.Schema](
        [b[-w, +r] <: Body.Node[w, r], w, r] =>
          (annotation: Annotation[Self.Record[Part.Schema[b, *, *], w, r]]) => new Multipart.Schema(annotation),
        [b[-w, +r] <: Body.Node[w, r], w, r] => (multipart: Multipart.Schema[b, w, r]) => multipart.self
      ):
    given recordable: [B[-w, +r] <: Body.Node[w, r]]
      => RecordableOperation[Multipart.Schema[B, *, *], Multipart.Schema[B, *, *]] =
      RecordableOperation.identity

    /** `parts :* part`. */
    given appendable: [B1[-w, +r] <: Body.Node[w, r], B2[-w, +r] <: Body.Node[w, r]]
        => AppendableOperation[
          Multipart.Schema[B1, *, *],
          Multipart.Schema[Multipart.Or[B1, B2], *, *],
          Part.Schema[B2, *, *]
        ]:
      override def lift[W, R](fa: Multipart.Schema[B1, W, R]): Multipart.Schema[Multipart.Or[B1, B2], W, R] = fa

      override def element[W, R](fb: => Part.Schema[B2, W, R]): Multipart.Schema[Multipart.Or[B1, B2], W, R] =
        Multipart.Schema.apply[Multipart.Or[B1, B2], W, R](Self.Record.Root(Reference.later(fb)))
