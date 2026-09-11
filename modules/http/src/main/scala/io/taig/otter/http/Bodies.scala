package io.taig.otter.http

import cats.data.Chain
import io.taig.otter as Self
import io.taig.otter.Annotation
import io.taig.otter.Reference
import io.taig.otter.Wrapper
import io.taig.otter.operation.*

/** A choice of bodies that round trips `A`. */
type Bodies[A] = Bodies.Of[Body.Payload, A]

object Bodies:
  /** Alternatives holding the payload `S` and round tripping `A`.
    *
    * A [[Self.Union]] of [[Body]] alternatives, which is what content negotiation is: writing picks the alternative
    * whose media type the caller will accept, and reading tries them until one fits. That the union is the same node a
    * JSON schema uses for a sum type is not a coincidence -- `Accept` and a discriminated union are the same question
    * asked of different bytes.
    */
  type Of[S[-w, +r], A] = Bodies.Schema[S, A, A]

  /** Holding anything, which is the form an interpreter is written against. */
  type Node = [w, r] =>> Bodies.Schema[Body.Payload, w, r]

  type Reader[+A] = Bodies.Reader.Of[Body.Payload, A]

  object Reader:
    type Of[S[-w, +r], +A] = Bodies.Schema[S, Nothing, A]

  type Writer[-A] = Bodies.Writer.Of[Body.Payload, A]

  object Writer:
    type Of[S[-w, +r], -A] = Bodies.Schema[S, A, Any]

  /** Every alternative the body offers, in the order it offers them. */
  def branches(schema: Bodies.Node[?, ?]): Chain[Body.Schema[?, ?, ?]] = Bodies.walk(schema.self.self)

  private def walk(schema: Self.Union[Body.Node, ?, ?]): Chain[Body.Schema[?, ?, ?]] = schema match
    case Self.Union.Modify(self, _, _)     => Bodies.walk(self)
    case Self.Union.Coproduct(left, right) => Bodies.walk(left) ++ Bodies.walk(right)
    case Self.Union.Root(branch)           => Chain.one(branch.value)

  /** Bodies that need not be sent at all.
    *
    * Not a fourth form a body comes in: optionality belongs to the request, and the two are genuinely different
    * questions, which [[Request.Value.OptionalPayload]] spells out -- a union says *which* entity arrived, and this
    * says whether one had to. It is a type of its own rather than a flag so that only a [[Request]] accepts one: a
    * [[Result]] has no overload taking it, and an answer that need not carry its entity stays a compile error rather
    * than a rule written down.
    *
    * It holds a [[Reference]] rather than the schema, which is what lets the position that takes one take it strictly:
    * the suspension every child position in this library needs is already here.
    */
  final case class Optional[+S[-w, +r], -W, +R](self: Reference[[w, r] =>> Bodies.Schema[S, w, r], W, R])

  final case class Schema[+S[-w, +r], -W, +R](
      self: Annotation[Self.Union[[w, r] =>> Body.Schema[S, w, r], W, R]]
  )

  object Schema
      extends Wrapper.Union[Body.Payload, Bodies.Schema, Body.Schema](
        [s[-w, +r], w, r] =>
          (annotation: Annotation[Self.Union[[a, b] =>> Body.Schema[s, a, b], w, r]]) => new Bodies.Schema(annotation),
        [s[-w, +r], w, r] => (bodies: Bodies.Schema[s, w, r]) => bodies.self
      ):
    given unionable: [S[-w, +r]]
      => UnionableOperation[[w, r] =>> Bodies.Schema[S, w, r], [w, r] =>> Bodies.Schema[S, w, r]] =
      UnionableOperation.identity

    /** `bodies :+ body`. */
    given alternable: [S1[-w, +r], S2[-w, +r]]
        => AlternableOperation[
          [w, r] =>> Bodies.Schema[S1, w, r],
          [w, r] =>> Bodies.Schema[Body.Or[S1, S2], w, r],
          [w, r] =>> Body.Schema[S2, w, r]
        ]:
      override def lift[W, R](fa: Bodies.Schema[S1, W, R]): Bodies.Schema[Body.Or[S1, S2], W, R] = fa

      override def element[W, R](fb: => Body.Schema[S2, W, R]): Bodies.Schema[Body.Or[S1, S2], W, R] =
        Bodies.Schema.apply[Body.Or[S1, S2], W, R](Self.Union.Root(Reference.later(fb)))
