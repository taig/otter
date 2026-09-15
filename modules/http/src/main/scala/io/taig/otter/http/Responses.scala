package io.taig.otter.http

import cats.data.Chain
import io.taig.otter as Self
import io.taig.otter.Annotation
import io.taig.otter.Reference
import io.taig.otter.Wrapper
import io.taig.otter.operation.*

/** Every answer an endpoint may give, round tripping `A`. */
type Responses[A] = Responses.Of[Body.Payload, A]

object Responses:
  /** Answers holding the payload `S` and round tripping `A`.
    *
    * A [[Self.Union]] of [[Response]]s, one per [[Status]]. The same node [[Bodies]] uses, one tier up: writing picks
    * the answer that matches, and reading tries them until one fits.
    */
  type Of[S[-w, +r], A] = Responses.Schema[S, A, A]

  /** Holding anything, which is the form an interpreter is written against. */
  type Node = [w, r] =>> Responses.Schema[Body.Payload, w, r]

  type Reader[+A] = Responses.Reader.Of[Body.Payload, A]

  object Reader:
    type Of[S[-w, +r], +A] = Responses.Schema[S, Nothing, A]

  type Writer[-A] = Responses.Writer.Of[Body.Payload, A]

  object Writer:
    type Of[S[-w, +r], -A] = Responses.Schema[S, A, Any]

  /** Every answer, in the order the endpoint names them. */
  def branches(schema: Responses.Node[?, ?]): Chain[Response.Schema[?, ?, ?]] = Responses.walk(schema.self.self)

  private def walk(schema: Self.Union[Response.Node, ?, ?]): Chain[Response.Schema[?, ?, ?]] = schema match
    case Self.Union.Modify(self, _, _)     => Responses.walk(self)
    case Self.Union.Coproduct(left, right) => Responses.walk(left) ++ Responses.walk(right)
    case Self.Union.Root(branch)           => Chain.one(branch.value)

  final case class Schema[+S[-_, +_], -W, +R](
      self: Annotation[Self.Union[Response.Schema[S, *, *], W, R]]
  )

  object Schema
      extends Wrapper.Union[Body.Payload, Responses.Schema, Response.Schema](
        [s[-w, +r], w, r] =>
          (annotation: Annotation[Self.Union[Response.Schema[s, *, *], w, r]]) => new Responses.Schema(annotation),
        [s[-w, +r], w, r] => (responses: Responses.Schema[s, w, r]) => responses.self
      ):
    given unionable: [S[-w, +r]]
      => UnionableOperation[Responses.Schema[S, *, *], Responses.Schema[S, *, *]] =
      UnionableOperation.identity

    /** `responses :+ response`. */
    given alternable: [S1[-w, +r], S2[-w, +r]]
        => AlternableOperation[
          Responses.Schema[S1, *, *],
          Responses.Schema[Body.Or[S1, S2], *, *],
          Response.Schema[S2, *, *]
        ]:
      override def lift[W, R](fa: Responses.Schema[S1, W, R]): Responses.Schema[Body.Or[S1, S2], W, R] = fa

      override def element[W, R](fb: => Response.Schema[S2, W, R]): Responses.Schema[Body.Or[S1, S2], W, R] =
        Responses.Schema.apply[Body.Or[S1, S2], W, R](Self.Union.Root(Reference.later(fb)))
