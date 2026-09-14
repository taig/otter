package io.taig.otter.http

import cats.Contravariant
import cats.Functor
import cats.Invariant
import cats.arrow.Profunctor
import io.taig.otter as Self
import io.taig.otter.Annotated
import io.taig.otter.Annotation
import io.taig.otter.Direction
import io.taig.otter.Metadata
import io.taig.otter.Reference
import io.taig.otter.operation.AlternableOperation
import io.taig.otter.operation.UnionableOperation

/** One of the answers with requirement `S`, round tripping `A`. */
type Response[S[-w, +r], A] = Response.Of[S, A]

object Response:
  /** A response holding the payload `S` and round tripping `A`.
    *
    * The same shape as a [[Request]] with the method and the path taken away and a [[Status]] put in their place, for
    * the same reason: an answer is a status, some headers and at most one entity.
    */
  type Of[S[-w, +r], A] = Response.Schema[S, A, A]

  /** Holding anything, which is the form an interpreter is written against. */
  type Node = [w, r] =>> Response.Schema[Body.Payload, w, r]

  type Reader[+A] = Response.Reader.Of[Body.Payload, A]

  object Reader:
    type Of[S[-w, +r], +A] = Response.Schema[S, Nothing, A]

  type Writer[-A] = Response.Writer.Of[Body.Payload, A]

  object Writer:
    type Of[S[-w, +r], -A] = Response.Schema[S, A, Any]

  final case class Schema[+S[-w, +r], -W, +R](self: Annotation[Response.Value[S, W, R]]):
    export self.self.{bodies, headers, status, streamed}

  object Schema:
    def apply[S[-w, +r], W, R](self: Response.Value[S, W, R]): Response.Schema[S, W, R] =
      new Response.Schema(Annotation(self))

    given annotated: [S[-w, +r], W, R] => Annotated[Response.Schema[S, W, R]]:
      extension (self: Response.Schema[S, W, R])
        override def lens: (Metadata, Metadata => Response.Schema[S, W, R]) =
          (self.self.metadata, metadata => new Response.Schema(self.self.copy(metadata = metadata)))

    given profunctor: [S[-w, +r]] => Profunctor[[w, r] =>> Response.Schema[S, w, r]]:
      override def dimap[W0, R0, W, R](
          self: Response.Schema[S, W0, R0]
      )(f: W => W0)(g: R0 => R): Response.Schema[S, W, R] =
        new Response.Schema(self.self.map(Response.Value.Modify(_, g, f)))

    given functor: [S[-w, +r]] => Functor[[a] =>> Response.Schema[S, Nothing, a]] =
      Direction.functor[[w, r] =>> Response.Schema[S, w, r]]

    given contravariant: [S[-w, +r]] => Contravariant[[a] =>> Response.Schema[S, a, Any]] =
      Direction.contravariant[[w, r] =>> Response.Schema[S, w, r]]

    given invariant: [S[-w, +r]] => Invariant[[a] =>> Response.Schema[S, a, a]] =
      Direction.invariant[[w, r] =>> Response.Schema[S, w, r]]

    given unionable: [S[-w, +r]]
      => UnionableOperation[[w, r] =>> Response.Schema[S, w, r], [w, r] =>> Responses.Schema[S, w, r]] =
      UnionableOperation.derived

    /** `response :+ response`. */
    given alternable: [S1[-w, +r], S2[-w, +r]]
        => AlternableOperation[
          [w, r] =>> Response.Schema[S1, w, r],
          [w, r] =>> Responses.Schema[Body.Or[S1, S2], w, r],
          [w, r] =>> Response.Schema[S2, w, r]
        ]:
      override def lift[W, R](fa: Response.Schema[S1, W, R]): Responses.Schema[Body.Or[S1, S2], W, R] =
        Responses.Schema.apply[Body.Or[S1, S2], W, R](Self.Union.Root(Reference.now(fa)))

      override def element[W, R](fb: => Response.Schema[S2, W, R]): Responses.Schema[Body.Or[S1, S2], W, R] =
        Responses.Schema.apply[Body.Or[S1, S2], W, R](Self.Union.Root(Reference.later(fb)))

  sealed abstract class Value[+S[-w, +r], -W, +R]:
    def status: Status

    def headers: Option[Reference[Headers.Node, ?, ?]]

    def bodies: Option[Reference[[w, r] =>> Bodies.Schema[S, w, r], ?, ?]]

    def streamed: Option[Reference[Body.Streamed.Node, ?, ?]]

  object Value:
    final case class Root(override val status: Status) extends Response.Value[Nothing, Unit, Unit]:
      override def headers: Option[Reference[io.taig.otter.http.Headers.Node, ?, ?]] = None

      override def bodies: Option[Reference[[w, r] =>> Bodies.Schema[Nothing, w, r], ?, ?]] = None

      override def streamed: Option[Reference[Body.Streamed.Node, ?, ?]] = None

    final case class Headers[+S[-w, +r], W1, R1, W2, R2](
        self: Response.Value[S, W1, R1],
        values: Reference[io.taig.otter.http.Headers.Node, W2, R2]
    ) extends Response.Value[S, (W1, W2), (R1, R2)]:
      export self.{bodies, status, streamed}

      override def headers: Option[Reference[io.taig.otter.http.Headers.Node, ?, ?]] = Some(values)

    final case class Entity[+S[-w, +r], W1, R1, W2, R2](
        self: Response.Value[S, W1, R1],
        values: Reference[[w, r] =>> Bodies.Schema[S, w, r], W2, R2]
    ) extends Response.Value[S, (W1, W2), (R1, R2)]:
      export self.{headers, status, streamed}

      override def bodies: Option[Reference[[w, r] =>> Bodies.Schema[S, w, r], ?, ?]] = Some(values)

    /** A streamed body added to a response, which changes what it describes without changing what it holds. */
    final case class Streamed[+S[-w, +r], W1, R1, W2, R2](
        self: Response.Value[Body.Streamed.Requirement[S], W1, R1],
        value: Reference[[w, r] =>> Body.Streamed.Schema[S, w, r], W2, R2]
    ) extends Response.Value[Body.Streamed.Requirement[S], W1, R1]:
      export self.{bodies, headers, status}

      override def streamed: Option[Reference[Body.Streamed.Node, ?, ?]] = Some(value)

    final case class Modify[+S[-w, +r], W0, R0, -W, +R](self: Response.Value[S, W0, R0], f: R0 => R, g: W => W0)
        extends Response.Value[S, W, R]:
      export self.{bodies, headers, status, streamed}
